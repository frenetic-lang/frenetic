# TODO: Turn this into a complete test suite and add to "make tests" and TravisCI

# Probably a better way to do this ...
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

