# TODO: Turn this into a complete test suite and add to "make tests" and TravisCI
# This makes sure we grab Frenetic from sandbox, rather than dsitribution
import sys
sys.path.append('../..')
import unittest
from frenetic.syntax import *

class SimpleTestCase(unittest.TestCase):
  def setUp(self):
    pass

SWITCH_2 = {'header': 'switch', 'type': 'test', 'value': '2'}

class SwitchEqSimpleTestCase(SimpleTestCase):
  def runTest(self):
    se = SwitchEq(2)
    self.assertEqual(se.to_json(), SWITCH_2)
    se_str = SwitchEq("2")
    self.assertEqual(se_str.to_json(), SWITCH_2)

class SwitchEqSimpleInvalidTestCases(SimpleTestCase):
  def runTest(self):
    se = self.assertRaises( AssertionError, SwitchEq, [2.3] )
    se = self.assertRaises( AssertionError, SwitchEq, [-1] )

SWITCH_2_OR_3 = { 
  'preds': [ 
      {'header': 'switch', 'type': 'test', 'value': '2'}, 
      {'header': 'switch', 'type': 'test', 'value': '3'} 
  ],
  'type': 'or'
}

class SwitchEqVarArgsTestCase(SimpleTestCase):
  def runTest(self):
    se = SwitchEq(2,3)
    self.assertEqual(se.to_json(), SWITCH_2_OR_3 )

class SwitchEqListTestCase(SimpleTestCase):
  def runTest(self):
    se = SwitchEq([2,3])
    self.assertEqual(se.to_json(), SWITCH_2_OR_3 )

IF_THEN_ELSE_CASE = {
  'pols': [
    {'pols': [
      {'pred': 
      {'header': 'location', 'type': 'test', 'value': {'type': 'physical', 'port': 8}}, 'type': 'filter'}, 
      {'header': 'location', 'type': 'mod', 'value': {'type': 'physical', 'port': 9}}
    ], 'type': 'seq'}, 
    {'pols': [
      {'pred': {'pred': {'header': 'location', 'type': 'test', 'value': {'type': 'physical', 'port': 8}}, 'type': 'neg'}, 'type': 'filter'}, 
      {'pred': {'type': 'false'}, 'type': 'filter'}
    ], 'type': 'seq'}
  ], 
  'type': 'union'
}

class IfThenElseTestCase(SimpleTestCase):
  def runTest(self):
    se = IfThenElse(PortEq(8), SetPort(9), drop)
    self.assertEqual(se.to_json(), IF_THEN_ELSE_CASE)

class SingleSetPort(SimpleTestCase):
  def runTest(self):
    sp = SetPort(2)
    self.assertEqual(sp.to_json(), {'header': 'location', 'type': 'mod', 'value': {'type': 'physical', 'port': 2}})

MULTIPLE_SET_PORT_CASE = {
  'pols': [
    {'header': 'location', 'type': 'mod', 'value': {'type': 'physical', 'port': 2}},
    {'header': 'location', 'type': 'mod', 'value': {'type': 'physical', 'port': 3}}
  ], 
  'type': 'union'
}

class MultipleSetPort(SimpleTestCase):
  def runTest(self):
    sp = SetPort(2,3)
    self.assertEqual(sp.to_json(), MULTIPLE_SET_PORT_CASE)

class ListSetPort(SimpleTestCase):
  def runTest(self):
    sp = SetPort([2,3])
    self.assertEqual(sp.to_json(), MULTIPLE_SET_PORT_CASE)

PACKET_OUT_MULTIPLE_PORTS = {
  "switch": 1, "in_port": 2, 
  "payload": { "type": "buffered", "bufferid": 666 },
  "actions": [
    { "type": "output", "pseudoport": { "type": "physical", "port": 1 } },
    { "type": "output", "pseudoport": { "type": "physical", "port": 2 } },
  ]
}

class PacketOutTraditional(SimpleTestCase):
  def runTest(self):
    ac = [ Output(Physical(1)), Output(Physical(2)) ]
    pko = PacketOut(1, Buffered(666, ""), ac, 2)
    self.assertEquals(pko.to_json(), PACKET_OUT_MULTIPLE_PORTS)

class PacketOutConvertModLocation(SimpleTestCase):
  def runTest(self):
    ac = [ Mod(Location(Physical(1))), Mod(Location(Physical(2))) ]
    pko = PacketOut(1, Buffered(666, ""), ac, 2)
    self.assertEquals(pko.to_json(), PACKET_OUT_MULTIPLE_PORTS)

class PacketOutConvertSetPort(SimpleTestCase):
  def runTest(self):
    ac = [ SetPort(1), SetPort(2) ]
    pko = PacketOut(1, Buffered(666, ""), ac, 2)
    self.assertEquals(pko.to_json(), PACKET_OUT_MULTIPLE_PORTS)    

class PacketOutConvertSetMultiPort(SimpleTestCase):
  def runTest(self):
    ac = [ SetPort(1,2) ]
    pko = PacketOut(1, Buffered(666, ""), ac, 2)
    self.assertEquals(pko.to_json(), PACKET_OUT_MULTIPLE_PORTS) 

class PacketOutConvertSetMultiPortList(SimpleTestCase):
  def runTest(self):
    ac = [ SetPort([1,2]) ]
    pko = PacketOut(1, Buffered(666, ""), ac, 2)
    self.assertEquals(pko.to_json(), PACKET_OUT_MULTIPLE_PORTS) 

PACKET_OUT_SEQUENCE = {
  "switch": 1, "in_port": 2, 
  "payload": { "type": "buffered", "bufferid": 666 },
  "actions": [
    { "header": "vlan", "type": "mod", "value": 1 },
    { "header": "vlanpcp", "type": "mod", "value": 3 }
  ]
}

class PacketOutSequenceTradtional(SimpleTestCase):
  def runTest(self):
    ac = [ SetVlan(1), SetVlanPcp(3) ]
    pko = PacketOut(1, Buffered(666, ""), ac, 2)
    self.assertEquals(pko.to_json(), PACKET_OUT_SEQUENCE) 

class PacketOutSequenceWithNetKATSeq(SimpleTestCase):
  def runTest(self):
    ac = Seq( [SetVlan(1), SetVlanPcp(3) ])
    pko = PacketOut(1, Buffered(666, ""), ac, 2)
    self.assertEquals(pko.to_json(), PACKET_OUT_SEQUENCE) 

