# -*- coding: utf-8 -*-

import test_pb2 as test
import google.protobuf.internal.encoder
from google.protobuf import message


msg = test.EnsimeMessage()
msg.test1.a = 150
msg.test1.b = u"great googly Ü moogly!"

msg.test2.z = 777
msg.test2.x = u"hello 中华人民共和"
msg.test2.y = True

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

