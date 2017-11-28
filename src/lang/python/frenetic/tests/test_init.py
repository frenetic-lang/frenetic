# Run as $ python -m unittest discover

# Probably a better way to do this ...
import sys
sys.path.append('../..')
from frenetic import *
from frenetic.syntax import *
from ryu.lib.packet import packet, ethernet, arp
from ryu.ofproto import ether
import unittest

class SimpleTestCase(unittest.TestCase):
  def setUp(self):
    pass

class PacketTestCase(SimpleTestCase):
  def runTest(self):
    app = App()

    # This is from coscin/coscin_app_ryu.  Simulates what you get in a PacketIn
    src_mac = "00:00:de:ad:be:ef"
    dst_mac = "ff:ff:ff:ff:ff:ff"
    src_ip = "192.168.56.100"
    dst_ip = "192.168.57.100"
    e = ethernet.ethernet(dst=dst_mac, src=src_mac, ethertype=ether.ETH_TYPE_ARP)
    pkt = arp.arp_ip(arp.ARP_REQUEST, src_mac, src_ip, "00:00:00:00:00:00", dst_ip)
    p = packet.Packet()
    p.add_protocol(e)
    p.add_protocol(pkt)
    p.serialize()
    payload = NotBuffered(binascii.a2b_base64(binascii.b2a_base64(p.data)))

    p_arp = app.packet(payload, "arp")
    self.assertEqual(p_arp.src_ip, src_ip )    

    p_eth = app.packet(payload, "ethernet")
    self.assertEqual(p_eth.src, src_mac )

    p_icmp = app.packet(payload, "icmp")
    self.assertEqual(p_icmp, None)    