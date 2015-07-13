import struct
from ryu.lib.packet import packet_base

class ProbeData(packet_base.PacketBase):

  PROBOCOL = 0x808
  NO_RESPONSE_THRESHOLD = 5
  _PACK_STR = '!QH'
  _MIN_LEN = struct.calcsize(_PACK_STR)
  _TYPE = {
      'ascii': [
          'src_switch', 'src_port'
      ]
  }

  def __init__(self, src_switch, src_port):
    self.src_switch = src_switch
    self.src_port = src_port

  @classmethod
  def parser(cls, buf):
      (src_switch, src_port) = struct.unpack_from(cls._PACK_STR, buf)
      return cls(src_switch, src_port), cls._TYPES.get(cls.PROBOCOL), buf[ProbeData._MIN_LEN:]

  def serialize(self, payload, prev):
      return struct.pack(ProbeData._PACK_STR, self.src_switch, self.src_port)

  def to_json(self):
    return { "src_switch": self.src_switch, "src_port" : self.src_port }

  def __eq__(self, other):
    return (isinstance(other, self.__class__) and
            self.src_switch == other.src_switch and
            self.src_port == other.src_port)

  def __hash__(self):
    return self.src_switch*29 + self.src_port*37

ProbeData.register_packet_type(ProbeData, ProbeData.PROBOCOL)
