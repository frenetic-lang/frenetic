# Abstract representation of a packet for use in PacketIn and PacketOut
import array, binascii
from frenetic.syntax import *
from frenetic.net_utils import *
from ryu.lib.packet import packet, ethernet, arp

class Packet(object):

  def __init__(self, 
    dpid = None, 
    port_id = None,   
    ethSrc = None,
    ethDst = None,
    ethType = None,
    vlan = None,
    vlanPcp = None,
    ip4Src = None,
    ip4Dst = None,
    ipProto = None,
    tcpSrcPort = None,
    tcpDstPort = None):

    self.switch = dpid
    self.port = port_id
    self.ethSrc = ethSrc
    self.ethDst = ethDst
    self.ethType = ethType
    self.vlan = vlan
    self.vlanPcp = vlanPcp
    self.ip4Src = ip4Src
    self.ip4Dst = ip4Dst
    self.ipProto = ipProto
    self.tcpSrcPort = tcpSrcPort
    self.tcpDstPort = tcpDstPort

    # This is all the leftover data
    self.unknown_headers = []

  # Class method for extracting a packet from a payload (packet in)
  @staticmethod
  def from_payload(dpid, port_id, payload):
    assert isinstance(payload, Payload)
    raw_data = payload.data

    # Parse the stuff out from any protocol
    ryu_pkt = packet.Packet(array.array('b', raw_data))

    pkt = Packet(dpid, port_id)

    for p in ryu_pkt.protocols:
      if p.protocol_name == 'ethernet':
        pkt.ethSrc = p.src
        pkt.ethDst = p.dst
        if p.ethertype != 0x8100:
          pkt.ethType = p.ethertype
      elif p.protocol_name == 'vlan':
        pkt.vlan = p.vid
        pkt.vlanPcp = p.pcp
        # When this is a Vlan packet, ethernet type comes
        # from the Inner packet, not the Vlan envelope
        pkt.ethType = p.ethertype
      elif p.protocol_name == 'ipv4':
        pkt.ip4Src = p.src
        pkt.ip4Dst = p.dst
        pkt.ipProto = p.proto
      elif p.protocol_name == 'tcp' or p.protocol_name == 'udp':
        pkt.tcpSrcPort = p.src_port
        pkt.tcpDstPort = p.dst_port
      elif p.protocol_name == 'arp':
        # The field aliasing is per Openflow 1.0 specs
        pkt.ipProto = p.opcode
        pkt.ip4Src = p.src_ip
        pkt.ip4Dst = p.dst_ip
      else:
        pkt.unknown_headers.append(p)

    return pkt

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

      # IP4Src and Dst matches can use a mask, so we handle them specially
      if header == "ip4src" or header == "ip4dst":
        # If there is no mask, the attribute
        # is not set, so this is the only way we can test for it
        try:
          mask = header_value.mask
        except AttributeError:
          mask = 32
        cidr_value = value + "/" + str(mask)
        pkt_value = self.get_header_value(header)
        return NetUtils.ip_in_network(pkt_value, cidr_value)
      else:
        return self.get_header_value(header) == value

    else:
      raise "unrecognized predicate: "+str(pred)

  def to_payload(self, ryu_packet_headers = []):
    # Do some sanity checks
    assert isinstance(ryu_packet_headers,list),"The ryu_packet_headers parameter must be a list"
    assert self.ethSrc != None, "ethSrc must be set"
    assert self.ethDst != None, "ethDst must be set"
    assert self.ethType != None, "ethType must be set"
    assert self.ethType != 0x8100, "For VLAN packets, set ethType to the inner packet type and set the vlan"
    if ((self.ethType == 0x800 or self.ethType == 0x806) or self.ip4Src != None or self.ip4Dst != None or self.ipProto != None):
      assert self.ip4Src != None, "For ARP/IP packets, ip4Src must be set"
      assert self.ip4Dst != None, "For ARP/IP packets, ip4Dst must be set"     
      assert self.ipProto != None, "For ARP/IP packets, ipProto must be set"     
      assert self.ethType == 0x800 or self.ethType == 0x806, "IP or ARP packets must have the proper ethType"

      if ((self.ipProto == 6 or self.ipProto == 17) or self.tcpSrcPort != None or self.tcpDstPort != None):
        assert self.tcpSrcPort != None, "tcpSrcPort must be set for TCP/UDP packets"
        assert self.tcpDstPort != None, "tcpDstPort must be set for TCP/UDP packets"
        assert self.ipProto == 6 or self.ipProto == 17, "TCP or UDP packets must have the proper ipProto"

    # Then put it together with RYU
    p = packet.Packet()
    used_etherType = 0x8100 if self.vlan != None else self.ethType
    e = ethernet.ethernet(dst=self.ethDst, src=self.ethSrc, ethertype=used_etherType)
    p.add_protocol(e)
    if (self.vlan != None):
      v = vlan.vlan(pcp=self.vlanPcp, vid=self.vlan, ethertype=self.ethType)
      p.add_protocol(v)
    if (self.ethType == 0x806):
      a = arp.arp(opcode=self.ipProto, src_ip=self.ip4Src, dst_ip=self.ip4Dst,
        src_mac=self.ethSrc, dst_mac=self.ethDst)
      p.add_protocol(a)
    elif (self.ethType == 0x800):
      i = ipv4.ipv4(proto=self.ipProto, src=self.ip4Src, dst=ip4Dst)
      p.add_protocol(i)
      if self.ipProto == 6:
        t = tcp.tcp(src_port=self.tcpSrcPort, dst_port=self.tcpDstPort)
        p.add_protocol(t)
      elif self.ipProto == 17:
        u = udp.udp(src_port=self.tcpSrcPort, dst_port=self.tcpDstPort)
        p.add_protocol(u)

    # Extra stuff for protocol headers not handled natively.
    for rph in ryu_packet_headers:
      p.add_protocol(rph)

    for uh in self.unknown_headers:
      p.add_protocol(uh)

    p.serialize()
    return NotBuffered(binascii.a2b_base64(binascii.b2a_base64(p.data)))

  def __str__(self):
    retval = "{ethSrc: "+self.ethSrc+", ethDst: "+self.ethDst+", ethType: "+str(self.ethType)
    if (self.vlan != None):
      retval += ", vlan: "+str(self.vlan)+", vlan: "+str(self.vlanPcp)
    if (self.ip4Src != None):
      retval += ", ip4Src: "+self.ip4Src+", ip4Dst: "+self.ip4Dst+", ipProto: "+str(self.ipProto)
    if (self.tcpSrcPort != None):
      retval += ", tcpSrcPort: "+str(self.tcpSrcPort)+", tcpDstPort: "+str(self.tcpDstPort)
    return retval + "}"
