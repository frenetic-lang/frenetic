""" Representation of NetKAT abstract syntax tree. """
import base64
import collections

class Action(object):
    pass

class Pseudoport(object):
    pass

class Physical(Pseudoport):

    def __init__(self, port):
        assert (port >= 1 and port <= 65535)
        self.port = port

    def to_json(self):
        return { "type": "physical", "port": self.port }

class Output(Action):

    def __init__(self, p):
        assert isinstance(p, Physical)
        self.pseudoport = p

    def to_json(self):
        return { "type": "output", "pseudoport": self.pseudoport.to_json() }

class Payload(object):
    pass

    @staticmethod
    def from_json(json):
        # TODO(arjun): fix names on the OCaml side
        if json['id'] == None:
            return NotBuffered(base64.b64decode(json['buffer']))
        else:
            return Buffered(json['id'], base64.b64decode(json['buffer']))


class Buffered(Payload):

    def __init__(self, buffer_id, data):
        assert type(buffer_id) == int and buffer_id >= 0
        self.buffer_id = buffer_id
        self.data = data

    def to_json(self):
        # NOTE(arjun): The data isn't being sent back to OCaml. But, it
        # doesn't matter, since the buffer ID is all that's needed to process
        # a buffered packet in a PACKET_OUT message.
        return { "type": "buffered", "bufferid": self.buffer_id }

class NotBuffered(Payload):

    def __init__(self, data):
        assert type(data) == str
        self.data = data

    def to_json(self):
        return { "type": "notbuffered", "data": base64.b64encode(self.data) }


class PacketOut(object):

    def __init__(self, switch, payload, actions, in_port = None):
        assert type(switch) == int and switch >= 0
        self.switch = switch
        assert isinstance(payload,Buffered) or isinstance(payload,NotBuffered)
        self.payload = payload
        #TODO: Can this be refined? Currently list<'instance'>
        assert isinstance(actions,list)
        self.actions = actions
        assert in_port == None or (type(in_port) == int and in_port >= 0)
        self.in_port = in_port

    def to_json(self):
        return { "switch": self.switch,
                 "in_port": self.in_port,
                 "actions": [ action.to_json() for action in self.actions ],
                 "payload": self.payload.to_json() }

class PacketIn(object):

    def __init__(self, json):
        assert (json['type'] == 'packet_in')
        assert type(json['switch_id']) == int and json['switch_id'] >= 0
        self.switch_id = json['switch_id']
        assert type(json['port_id']) == int and json['switch_id'] >= 0
        self.port_id = json['port_id']
        # TODO: assert isinstance(payload,Buffered) or isinstance(payload,NotBuffered)
        self.payload = Payload.from_json(json['payload'])


class Stats(object):

    def __init__(self, packets, bytes):
        self.packets = packets
        self.bytes = bytes

    @staticmethod
    def from_json(json):
        packets = json["packets"]
        bytes = json["bytes"]
        Stats(packets, bytes)

    def __str__(self):
        return "Stats(packets=%s,bytes=%s)" % (self.packets, self.bytes)

################################################################################
#
# NetKAT Serialization
#
# This is based on Frenetic_NetKAT_Json.ml
################################################################################

class Pred(object):
    """ A class to represent NetKAT predicates. This class's constructor should
    not be called directly. Instead, the user should construct objects using
    the various convenience functions defined in this module, together with the
    class's overloaded operators such as `&`, `|`, and `~` for conjunction,
    disjunction, and negation, respectively. """

    def __and__(self, other):
        return And([self, other])

    def __or__(self, other):
        return Or([self, other])

    def __invert__(self):
        return Not(self)

    def ite(self, true_pol, false_pol):
        return (Filter(self) >> true_pol) | (Filter(~self) >> false_pol)

class And(Pred):

    def __init__(self, children):
        children = list(children)
        assert (isinstance(pol,Pred) for pol in children)
        self.children = children

    def to_json(self):
        return {
          "type": "and",
          "preds": [ pred.to_json() for pred in self.children ]
        }

class Or(Pred):

    def __init__(self, children):
        children = list(children)
        assert (isinstance(pol,Pred) for pol in children)
        self.children = children

    def to_json(self):
        return {
          "type": "or",
          "preds": [ pred.to_json() for pred in self.children ]
        }

class Not(Pred):

    def __init__(self, pred):
        assert isinstance(pred,Pred)
        self.pred = pred

    def to_json(self):
        return {
          "type": "neg",
          "pred": self.pred.to_json()
        }

class Id(Pred):

    def to_json(self):
        return { "type": "true" }

class Drop(Pred):

    def to_json(self):
        return { "type": "false" }

class Test(Pred):

    def __init__(self, hv):
        self.hv = hv

    def to_json(self):
        return {
          "type": "test",
          "header": self.hv.header,
          "value": self.hv.value_to_json()
        }

class Policy(object):

    def __rshift__(self, other):
        return Seq([self, other])

    def __or__(self, other):
        return Union([self, other])

class Filter(Policy):

    def __init__(self, pred):
        assert isinstance(pred,Pred)
        self.pred = pred

    def to_json(self):
        return { "type": "filter", "pred": self.pred.to_json() }

class HeaderAndValue(object):
    pass

class Switch(HeaderAndValue):

    def __init__(self, value):
        self.header = "switch"
        assert type(value) == int and value >= 0
        self.value = value

    def value_to_json(self):
        # JSON doesn't allow 64-bit ints, so we convert them to strings.  Frenetic converts them back.  
        return str(self.value)

class Query(object):

    def __init__(self, name):
        self.name = name

    def to_json(self):
        return { "type": "query", "name": self.name }

class Pipe(object):

    def __init__(self, name):
        self.name = name

    def to_json(self):
        return { "type": "pipe", "name": self.name }

class Physical(object):

    def __init__(self, port):
        assert type(port) == int and port >= 0
        self.port = port

    def to_json(self):
        return { "type": "physical", "port": self.port }

class Location(HeaderAndValue):

    def __init__(self, value):
        self.header = "location"
        assert isinstance(value,Physical) or isinstance(value,Pipe) or isinstance(value, Query)
        self.value = value

    def value_to_json(self):
        return self.value.to_json()

class EthSrc(HeaderAndValue):

    def __init__(self, value):
        self.header = "ethsrc"
        assert type(value) == str or type(value) == unicode
        self.value = value

    def value_to_json(self):
        return self.value

class EthDst(HeaderAndValue):

    def __init__(self, value):
        self.header = "ethdst"
        assert type(value) == str or type(value) == unicode
        self.value = value

    def value_to_json(self):
        return self.value

class Vlan(HeaderAndValue):

    def __init__(self, value):
        self.header = "vlan"
        assert type(value) == int and value >= 0
        self.value = value

    def value_to_json(self):
        return self.value

class VlanPcp(HeaderAndValue):

    def __init__(self, value):
        self.header = "vlanpcp"
        assert type(value) == int and value >= 0
        self.value = value

    def value_to_json(self):
        return self.value

class EthType(HeaderAndValue):

    def __init__(self, value):
        self.header = "ethtype"
        assert type(value) == int and value >= 0
        self.value = value

    def value_to_json(self):
        return self.value

class IPProto(HeaderAndValue):

    def __init__(self, value):
        self.header = "ipproto"
        assert type(value) == int and value >= 0
        self.value = value

    def value_to_json(self):
        return self.value

# TODO: import ipaddress to check validity?
class IP4Src(HeaderAndValue):

    def __init__(self, value, mask = None):
        self.header = "ip4src"
        assert type(value) == str or type(value) == unicode
        if mask != None:
            assert type(mask) == int
            self.mask = mask
        self.value = value

    def value_to_json(self):
        try:
            if self.mask != None:
                return { "addr": self.value, "mask": self.mask }
        except AttributeError:
            return { "addr": self.value, "mask": 32 }

class IP4Dst(HeaderAndValue):

    def __init__(self, value, mask = None):
        self.header = "ip4dst"
        assert type(value) == str or type(value) == unicode
        if mask != None:
            assert type(mask) == int
            self.mask = mask
        self.value = value

    def value_to_json(self):
        try:
            if self.mask != None:
                return { "addr": self.value, "mask": self.mask }
        except AttributeError:
            return { "addr": self.value, "mask": 32 }

class TCPSrcPort(HeaderAndValue):

    def __init__(self, value):
        self.header = "tcpsrcport"
        assert type(value) == int and value >= 0
        self.value = value

    def value_to_json(self):
        return self.value

class TCPDstPort(HeaderAndValue):

    def __init__(self, value):
        self.header = "tcpdstport"
        assert type(value) == int and value >= 0
        self.value = value

    def value_to_json(self):
        return self.value

class Mod(Policy):

    def __init__(self, hv):
        assert isinstance(hv,HeaderAndValue)
        self.hv = hv

    def to_json(self):
        return {
          "type": "mod",
          "header": self.hv.header,
          "value": self.hv.value_to_json()
        }

class Union(Policy):

    def __init__(self, children):
        children = list(children)
        for pol in children:
          assert isinstance(pol, Policy)
        self.children = children

    def to_json(self):
        return {
          "type": "union",
          "pols": [ pol.to_json() for pol in self.children ]
        }

class Seq(Policy):

    def __init__(self, children):
        children = list(children)

        for pol in children:
          assert isinstance(pol, Policy)

        self.children = children

    def to_json(self):
        return {
          "type": "seq",
          "pols": [ pol.to_json() for pol in self.children ]
        }

class IfThenElse(Policy):
    def __init__(self, pred, true_pol, false_pol):
        assert isinstance(pred, Pred) and isinstance(true_pol, Policy) and isinstance(false_pol, Policy)
        self.policy = pred.ite(true_pol, false_pol)

    def to_json(self):
        return self.policy.to_json()

# Shorthands

true = Id()
false = Drop()
id = Filter(true)
drop = Filter(false)

# These classes are syntactic sugar to reduce the number of parantheses in 
# Net apps.  They're pretty redundant, but since the NetKAT syntax doesn't 
# change often, deupification is not really warranted.

########## ___Eq

# Given a field class name (Switch, Port, ...) and a value, return the expanded predicate
# The value can be a single int value, a var args list of ints, a real python list of ints,
# or the equivalent string values

def init_int_eq(klass, *values):
    if type(values[0]) == list:
        values = values[0]
    vs = []
    for v in values:
        if type(v) == str:
            v = int(v)
        assert(type(v) == int and v >= 0)
        vs.append(v)
    expanded_preds = [ Test(klass(v)) for v in vs ]
    if len(expanded_preds) > 1:
        return Or(expanded_preds)
    elif len(expanded_preds) == 0:
        return false
    else:
        return expanded_preds[0]

def init_str_eq(klass, *values):
    if type(values[0]) == list:
        values = values[0]
    for v in values:
        assert(type(v) == str or type(value) == unicode)
    expanded_preds = [ Test(klass(v)) for v in values ]
    if len(expanded_preds) > 1:
        return Or(expanded_preds)
    elif len(expanded_preds) == 0:
        return false
    else:
        return expanded_preds[0]

class MultiPred(Pred):
    def to_json(self):
        return self.hv.to_json()

class SwitchEq(MultiPred):
    def __init__(self, *values):
        self.hv = init_int_eq(Switch, *values)

class PortEq(MultiPred):
    def __init__(self, *values):
        # You have to do it this way because Location(Physical()) is > 1 class.
        # Also we do more stringent value checking
        if type(values[0]) == list:
            values = values[0]
        vs = []
        for v in values:
            if type(v) == str:
                v = int(v)
            assert(type(v) == int and v >= 1 and v <= 65535)
            vs.append(v)
        expanded_preds = [ Test(Location(Physical(v))) for v in vs ]
        if len(expanded_preds) > 1:
            self.hv = Or(expanded_preds)
        else:
            self.hv = expanded_preds[0]

class EthSrcEq(MultiPred):
    def __init__(self, *values):
        self.hv = init_str_eq(EthSrc, *values)

class EthDstEq(MultiPred):
    def __init__(self, *values):
        self.hv = init_str_eq(EthDst, *values)

class VlanEq(MultiPred):
    def __init__(self, *values):
        self.hv = init_int_eq(Vlan, *values)

class VlanPcpEq(MultiPred):
    def __init__(self, *values):
        self.hv = init_int_eq(VlanPcp, *values)

class EthTypeEq(MultiPred):
    def __init__(self, *values):
        self.hv = init_int_eq(EthType, *values)

class IPProtoEq(MultiPred):
    def __init__(self, *values):
        self.hv = init_int_eq(IPProto, *values)

class TCPSrcPortEq(MultiPred):
    def __init__(self, *values):
        self.hv = init_int_eq(TCPSrcPort, *values)

class TCPDstPortEq(MultiPred):
    def __init__(self, *values):
        self.hv = init_int_eq(TCPDstPort, *values)

# IP4Src and IP4Dst are the only ones that can't be used with a list or varags because
# the mask makes it too unpredictable

class IP4SrcEq(MultiPred):
    def __init__(self, value, mask = None):
        assert(type(value) == str or type(value == unicode))
        if mask != None:
            assert type(mask) == int
        self.hv = Test(IP4Src(value, mask))

    def to_json(self):
        return self.hv.to_json()

class IP4DstEq(MultiPred):
    def __init__(self, value, mask = None):
        assert(type(value) == str or type(value == unicode))
        if mask != None:
            assert type(mask) == int
        self.hv = Test(IP4Dst(value, mask))

    def to_json(self):
        return self.hv.to_json()


########## ___NotEq

class SwitchNotEq(MultiPred):
    def __init__(self, *values):
        self.hv = Not(SwitchEq(*values))

class PortNotEq(MultiPred):
    def __init__(self, *values):
        self.hv = Not(PortEq(*values))

class EthSrcNotEq(MultiPred):
    def __init__(self, *values):
        self.hv = Not(EthSrcEq(*values))

class EthDstNotEq(MultiPred):
    def __init__(self, *values):
        self.hv = Not(EthDstEq(*values))

class VlanNotEq(MultiPred):
    def __init__(self, *values):
        self.hv = Not(VlanEq(*values))

class VlanPcpNotEq(MultiPred):
    def __init__(self, *values):
        self.hv = Not(VlanPcpEq(*values))

class EthTypeNotEq(MultiPred):
    def __init__(self, *values):
        self.hv = Not(EthTypeEq(*values))

class IPProtoNotEq(MultiPred):
    def __init__(self, *values):
        self.hv = Not(IPProtoEq(*values))

class IP4SrcNotEq(MultiPred):
    def __init__(self, value, mask = None):
        self.hv = Not(IP4SrcEq(value, mask))

class IP4DstNotEq(MultiPred):
    def __init__(self, value, mask = None):
        self.hv = Not(IP4DstEq(value, mask))

class TCPSrcPortNotEq(MultiPred):
    def __init__(self, *values):
        self.hv = Not(TCPSrcPortEq(*values))

class TCPDstPortNotEq(MultiPred):
    def __init__(self, *values):
        self.hv = Not(TCPDstPortEq(*values))

########## Set___

def int_policy(klazz, value):
    if (type(value)) == str:
        value = int(value)
    assert(type(value) == int and value > 0)
    return Mod(klazz(value))

def str_policy(klazz, value):
    assert(type(value) == str or type(value == unicode))
    return Mod(klazz(value))

class SinglePolicy(Policy):
    def to_json(self):
        return self.hv.to_json()

# With SetPort we do extra checking to make sure the port id is legal OpenFlow
class SetPort(SinglePolicy):
    def __init__(self, value):
        if (type(value)) == str:
            value = int(value)
        assert(type(value) == int and value >= 1 and value <= 65535)
        self.hv = Mod(Location(Physical(value)))

class SetEthSrc(SinglePolicy):
    def __init__(self, value):
        self.hv = str_policy(EthSrc, value)

class SetEthDst(SinglePolicy):
    def __init__(self, value):
        self.hv = str_policy(EthDst, value)

class SetVlan(SinglePolicy):
    def __init__(self, value):
        self.hv = int_policy(Vlan, value)

class SetVlanPcp(SinglePolicy):
    def __init__(self, value):
        self.hv = int_policy(VlanPcp, value)

class SetEthType(SinglePolicy):
    def __init__(self, value):
        self.hv = int_policy(EthType, value)

class SetIPProto(SinglePolicy):
    def __init__(self, value):
        self.hv = int_policy(IPProto, value)

class SetTCPSrcPort(SinglePolicy):
    def __init__(self, value):
        self.hv = int_policy(TCPSrcPort, value)

class SetTCPDstPort(SinglePolicy):
    def __init__(self, value):
        self.hv = int_policy(TCPDstPort, value)

# IP4Src and Ip4Dst are weird because they may contain a mask
class SetIP4Src(SinglePolicy):
    def __init__(self, value, mask = None):
        assert(type(value) == str or type(value == unicode))
        if mask != None:
            assert type(mask) == int
        self.hv = Mod(IP4Src(value, mask))

class SetIP4Dst(SinglePolicy):
    def __init__(self, value, mask = None):
        assert(type(value) == str or type(value == unicode))
        if mask != None:
            assert type(mask) == int
        self.hv = Mod(IP4Dst(value, mask))

############### Misc.

class SendToController(SinglePolicy):
    def __init__(self, value):
        assert(type(value) == str or type(value == unicode))
        self.hv = Mod(Location(Pipe(value)))

class CompilerOptions:

    def __init__(self, cache_prepare, field_order, remove_tail_drops, dedup_flows, optimize):
        assert type(cache_prepare) == str
        assert type(field_order) == str
        assert type(remove_tail_drops) == bool
        assert type(dedup_flows) == bool
        assert type(optimize) == bool

        self.cache_prepare = cache_prepare
        self.field_order = field_order
        self.remove_tail_drops = remove_tail_drops
        self.dedup_flows = dedup_flows
        self.optimize = optimize

    def to_json(self):
        return {
            "cache_prepare": self.cache_prepare,
            "field_order": self.field_order,
            "remove_tail_drops": self.remove_tail_drops,
            "dedup_flows": self.dedup_flows,
            "optimize": self.optimize
        }


