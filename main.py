# -*- coding: utf-8 -*-

import ensime_pb2 as ensime
import google.protobuf.internal.encoder
from google.protobuf import message


msg = ensime.WireMessage()
msg.call.call_id = 1
msg.call.init_project.root_dir = u"/home/aemon/tmp"
msg.call.init_project.active_subproject = u"/hullo/there"
msg.call.init_project.config.name = "test_project"

def write_varint(out, value):
    bits = value & 0x7f
    value >>= 7
    while value:
      out.write(chr(0x80|bits))
      bits = value & 0x7f
      value >>= 7
    out.write(chr(bits))

with open("out", "wb") as f:
    write_varint(f, msg.ByteSize())
    f.write(msg.SerializeToString())

