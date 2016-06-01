# Abstract representation of a packet for use in PacketIn and PacketOut
import array
from frenetic.syntax import *
from ryu.lib.packet import packet, ethernet, arp

class Packet(object):

  def __init__(self, dpid, port_id, payload):
    if isinstance(payload, Payload):
      raw_data = payload.data

      # Parse the stuff out from any protocol
      pkt = packet.Packet(array.array('b', raw_data))

      # We start with a clean slate to accomodate all different packet types
      self.switch = dpid
      self.port = port_id
      self.ethSrc = None
      self.ethDst = None
      self.ethType = None
      self.vlan = None
      self.vlanPcp = None
      self.ip4Src = None
      self.ip4Dst = None
      self.ipProto = None
      self.tcpSrcPort = None
      self.tcpDstPort = None
      self.arpOpcode = None
      self.arpSrcMac = None
      self.arpSrcIp = None
      self.arpDstMac = None
      self.arpDstIp = None

      for p in pkt:
        if p.protocol_name == 'ethernet':
          self.ethSrc = p.src
          self.ethDst = p.dst
          if p.ethertype != 0x8100:
            self.ethType = p.ethertype
        elif p.protocol_name == 'vlan':
          self.vlan = p.vid
          self.vlanPcp = p.pcp
          # When this is a Vlan packet, ethernet type comes
          # from the Inner packet, not the Vlan envelope
          self.ethType = p.ethertype
        elif p.protocol_name == 'ipv4':
          self.ip4Src = p.src
          self.ip4Dst = p.dst
          self.ipProto = p.proto
        elif p.protocol_name == 'tcp' or p.protocol_name == 'udp':
          self.tcpSrcPort = p.src_port
          self.tcpDstPort = p.dst_port
        elif p.protocol_name == 'arp':
          # These are not part of the OpenFlow match protocol, but
          # they're used so often, we include them
          self.arpOpcode = p.opcode
          self.arpSrcMac = p.src_mac
          self.arpSrcIp = p.src_ip
          self.arpDstMac = p.dst_mac
          self.arpDstIp = p.dst_ip

  # Translates JSON attribute names to NetKAT-compatible ones
  TRANSLATE_JSON_ATTRIBUTE_NAMES = {
    "switch": "switch",
    "location": "port",
    "ethsrc": "ethSrc",
    "ethdst": "ethDst",
    "ethtype": "ethType",
    "vlan": "vlan",
    "vlanpcp": "vlanPcp",
    "ip4src": "ip4Src",
    "ip4dst": "ip4Dst",
    "ipproto": "ipProto",
    "tcpsrcport": "tcpSrcPort",
    "tcpdstport": "tcpDstPort",
  }

  def get_header_value(self, header_name):
    return getattr(self, self.TRANSLATE_JSON_ATTRIBUTE_NAMES[header_name])

  def matches(self, pred):
    # Easy cases
    if isinstance(pred, Id):
      return True
    elif isinstance(pred, Drop):
      return False

    # Boolean operators
    elif isinstance(pred, And):
      # Use short-circuit evaluation
      for p in pred.children:
        if not self.matches(p):
          return False
      return True
    elif isinstance(pred, Or):
      # Use short-circuit evaluation
      for p in pred.children:
        if self.matches(p):
          return True
      return False
    elif isinstance(pred, Not):
      return not self.matches(pred.pred)

    # Base cases
    elif isinstance(pred, MultiPred):
      return self.matches(pred.hv)
    elif isinstance(pred, Test):
      header_value = pred.hv
      header = header_value.header
      value = header_value.value
      # Location is a special case.  An incoming packet will only match a physical port
      if header == "location":
        value = value.port
      return self.get_header_value(header) == value

    else:
      raise "unrecognized predicate: "+str(pred)