import sys
import re
from google.protobuf.internal import wire_format
from google.protobuf.descriptor_pb2 import FieldDescriptorProto as field_proto

import imp
pb = imp.load_source('pb', "%s.py" % sys.argv[1])

_MSGS = []

for msg in pb.DESCRIPTOR.message_types_by_name.values():
    _MSGS.append(msg)
    for msg in msg.nested_types:
        _MSGS.append(msg)

def _lisp_name(name):
    return _camel_to_underscore(
        name.replace(pb.DESCRIPTOR.package, "").replace(".", "")
        ).replace("_", "-")

def _camel_to_underscore(name):
    s1 = re.sub('(.)([A-Z][a-z]+)', r'\1_\2', name)
    return re.sub('([a-z0-9])([A-Z])', r'\1_\2', s1).lower()

def main(out):

    def add_package(s):
        return s.replace("pbel", sys.argv[3] + "-pbel")

    def ln(s):
        out.write(add_package(s) + "\n")

    with open(sys.argv[2], "r") as f:
        out.write(add_package(f.read()))
    ln("")
    ln(";; Start generated code here:")
    ln("")

    def read_field_case(field, read_func):
        ln("        ((= %d num) (setq result (cons :%s (cons (pbel-read-%s) result))))" %
           (field.number, _lisp_name(field.name), read_func))

    for i, msg in enumerate(_MSGS):
        ln("(defun pbel-read-%s ()" % _lisp_name(msg.full_name))
        ln("  \"Read message of type %s from buffer.\"" % msg.name)
        ln("  (let ((j (+ i (pbel-read-varint)))")
        ln("        (result nil))")
        ln("    (while (< i j)")
        ln("      (let* ((key (pbel-read-varint))")
        ln("             (num (pbel-field-number key)))")
        ln("        (cond")
        for field in msg.fields:
            if field.type == field_proto.TYPE_UINT32:
                read_field_case(field, "varint")
            elif field.type == field_proto.TYPE_BOOL:
                read_field_case(field, "bool")
            elif field.type == field_proto.TYPE_STRING:
                read_field_case(field, "string")
            elif field.type == field_proto.TYPE_MESSAGE:
                read_field_case(field, _lisp_name(field.message_type.full_name))
            else:
                raise StandardError("Unknown field type: " + str(field.type))
        ln("        (t (pbel-skip (pbel-wire-type key)))")
        ln("   ))) result))")
        ln("")

    def wire_type(field):
        if field.type == field_proto.TYPE_UINT32:
            return wire_format.WIRETYPE_VARINT
        elif field.type == field_proto.TYPE_BOOL:
            return wire_format.WIRETYPE_VARINT
        elif field.type == field_proto.TYPE_STRING:
            return wire_format.WIRETYPE_LENGTH_DELIMITED
        elif field.type == field_proto.TYPE_MESSAGE:
            return wire_format.WIRETYPE_LENGTH_DELIMITED
        else:
            raise "Unknown field type!"

    def write_field_case(field, write_func):
        ln("    (when (plist-get msg :%s)" % _lisp_name(field.name))
        ln("      (pbel-write-varint %d)" %
           ((field.number << 3) | wire_type(field)))
        ln("      (pbel-write-%s (plist-get msg :%s)))" %
           (write_func, _lisp_name(field.name)))

    for i, msg in enumerate(_MSGS):
        ln("(defun pbel-write-%s (msg)" % _lisp_name(msg.full_name))
        ln("  \"Write message of type %s to buffer.\"" % msg.name)
        ln("  (let ((start-pt (point))")
        ln("        (end-pt))")
        for field in msg.fields:
            if field.type == field_proto.TYPE_UINT32:
                write_field_case(field, "varint")
            elif field.type == field_proto.TYPE_BOOL:
                write_field_case(field, "bool")
            elif field.type == field_proto.TYPE_STRING:
                write_field_case(field, "string")
            elif field.type == field_proto.TYPE_MESSAGE:
                write_field_case(field, _lisp_name(field.message_type.full_name))
            else:
                raise StandardError("Unknown field type: " + str(field.type))
        ln("    (setq end-pt (point))")
        ln("    (goto-char start-pt)")
        ln("    (pbel-write-varint i)")
        ln("    (forward-char (- end-pt start-pt))")
        ln("  ))")

    ln("")
    ln("")
    ln("(provide 'pbel)")





#generate a parser that reads an ensimemessage from buffer:
#  use msg type to jump to function for reading sub message
#
#for each message type:
#    genearate custom function for reading each message type:
#      gen loop that reads each field, wire_type, field_number
#         gen case switch on field_number
#           foreach field in descriptor:
#             gen case 'name': (setq val (cons 'name' (cons (pbel-read-string) val)))
#             ...
#             gen case 'id': (pbel-read-varint)
#             gen case 'x': (pbel-read-varint)
#             gen case unknown: (pbel-skip wire-type)
#


if __name__ == "__main__":
    main(sys.stdout)
