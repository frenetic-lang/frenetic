# This makes sure we grab Frenetic from sandbox, rather than dsitribution
import sys, binascii
sys.path.append('../..')
import unittest
from frenetic.syntax import *
from frenetic.packet import Packet
from ryu.lib.packet import packet, ethernet, arp, ipv4, tcp
from ryu.ofproto import ether

class SimpleTestCase(unittest.TestCase):
  def setUp(self):
    pass

ETHERNET_DATA = {
  "dst_mac": "00:01:02:03:04:05",
  "src_mac": "05:04:03:02:01:00",
  "ethertype": 0x8888
}

def sampleEthernetPayload():
  e = ethernet.ethernet(
    dst=ETHERNET_DATA["dst_mac"], src=ETHERNET_DATA["src_mac"], ethertype=ETHERNET_DATA["ethertype"]
  )
  p = packet.Packet()
  p.add_protocol(e)
  p.serialize()
  return NotBuffered(binascii.a2b_base64(binascii.b2a_base64(p.data)))

TCP_DATA = {
  "ethertype": 0x0800,
  "src_ip": "192.168.0.100",
  "dst_ip": "192.168.0.101",
  "src_port": 16789,
  "dst_port": 80,
  "proto": 6
}

def sampleTcpPayload():
  e = ethernet.ethernet(
    dst=ETHERNET_DATA["dst_mac"], src=ETHERNET_DATA["src_mac"], ethertype=TCP_DATA["ethertype"]
  )
  pip = ipv4.ipv4(src=TCP_DATA["src_ip"], dst=TCP_DATA["dst_ip"], proto=TCP_DATA["proto"])
  ptcp = tcp.tcp(src_port=TCP_DATA["src_port"], dst_port=TCP_DATA["dst_port"])
  p = packet.Packet()
  p.add_protocol(e)
  p.add_protocol(pip)
  p.add_protocol(ptcp)
  p.serialize()
  return NotBuffered(binascii.a2b_base64(binascii.b2a_base64(p.data)))

class ParseEthernetPacket(SimpleTestCase):
  def runTest(self):
    payload = sampleEthernetPayload()
    pkt = Packet.from_payload(1,2,payload)
    self.assertEquals(pkt.switch, 1)
    self.assertEquals(pkt.port, 2)
    self.assertEquals(pkt.ethDst, ETHERNET_DATA["dst_mac"])
    self.assertEquals(pkt.ethSrc, ETHERNET_DATA["src_mac"])
    self.assertEquals(pkt.ethType, ETHERNET_DATA["ethertype"])
    self.assertEquals(pkt.ip4Src, None)

class ParseTcpPacket(SimpleTestCase):
  def runTest(self):
    payload = sampleTcpPayload()
    pkt = Packet.from_payload(1,2,payload)
    self.assertEquals(pkt.switch, 1)
    self.assertEquals(pkt.port, 2)
    self.assertEquals(pkt.ethDst, ETHERNET_DATA["dst_mac"])
    self.assertEquals(pkt.ethSrc, ETHERNET_DATA["src_mac"])
    self.assertEquals(pkt.ethType, TCP_DATA["ethertype"])
    self.assertEquals(pkt.ip4Src, TCP_DATA["src_ip"])
    self.assertEquals(pkt.ip4Dst, TCP_DATA["dst_ip"])
    self.assertEquals(pkt.ipProto, TCP_DATA["proto"])
    self.assertEquals(pkt.tcpSrcPort, TCP_DATA["src_port"])
    self.assertEquals(pkt.tcpDstPort, TCP_DATA["dst_port"])

class MatchesEthernetPacket(SimpleTestCase):
  def runTest(self):
    payload = sampleEthernetPayload()
    pkt = Packet.from_payload(1,2,payload)
    self.assertTrue(pkt.matches(SwitchEq(1)))
    self.assertFalse(pkt.matches(SwitchEq(2))) 
    self.assertTrue(pkt.matches(EthSrcEq( ETHERNET_DATA["src_mac"] )))
    self.assertTrue(pkt.matches(PortEq(2)))
    self.assertTrue(pkt.matches(SwitchEq(1) & PortEq(2)))
    self.assertTrue(pkt.matches(SwitchEq(23452345) | PortEq(2)))
    self.assertTrue(pkt.matches(~ SwitchEq(23452345)))

class MatchesTcpPacket(SimpleTestCase):
  def runTest(self):
    payload = sampleTcpPayload()
    pkt = Packet.from_payload(1,2,payload)
    self.assertTrue(pkt.matches(TCPSrcPortEq(TCP_DATA["src_port"]) & TCPDstPortEq(TCP_DATA["dst_port"])))
    self.assertTrue(pkt.matches(IP4SrcEq(TCP_DATA["src_ip"])))
    # Network matches
    self.assertTrue(pkt.matches(IP4SrcEq("192.168.0.0",24)))
    self.assertTrue(pkt.matches(IP4DstEq("192.168.0.101",32)))
    self.assertTrue(pkt.matches(IP4DstEq("192.168.0.0",16)))
