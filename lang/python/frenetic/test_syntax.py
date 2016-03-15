import unittest
from syntax import *

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
