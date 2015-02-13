""" Representation of NetKAT abstract syntax tree. """
import base64
import collections

class Action:
    pass

class Pseudoport:
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
        return { "type": "output", "Pseudoport": self.pseudoport.to_json() }

class Payload:
    pass

    @staticmethod
    def from_json(json):
        # TODO(arjun): fix names on the OCaml side
        if json['id'] == None:
            return NotBuffered(base64.b64decode(json['buffer']))
        else:
            # TODO(arjun): may have some bytes. do not discard
            return Buffered(json['id'])


class Buffered(Payload):

    def __init__(self, buffer_id):
        assert type(buffer_id) == int and buffer_id >= 0
        self.buffer_id = buffer_id

    def to_json(self):
        return { "type": "Buffered", "bufferid": self.buffer_id }

class NotBuffered(Payload):

    def __init__(self, data):
        assert type(data) == str
        self.data = data

    def to_json(self):
        return { "type": "NotBuffered", "data": base64.b64encode(self.data) }


class PacketOut():

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

class PacketIn():

    def __init__(self, json):
        assert (json['type'] == 'packet_in')
        assert type(json['switch_id']) == int and json['switch_id'] >= 0
        self.switch_id = json['switch_id']
        assert type(json['port_id']) == int and json['switch_id'] >= 0
        self.port_id = json['port_id']
        # TODO: assert isinstance(payload,Buffered) or isinstance(payload,NotBuffered)
        self.payload = Payload.from_json(json['payload'])

################################################################################
#
# NetKAT Serialization
#
# This is based on NetKAT_Json.ml
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

class And(Pred):

    def __init__(self, children):
        for a in children:
          assert isinstance(a,Pred)
        self.children = children

    def to_json(self):
        return {
          "type": "and",
          "preds": [ pred.to_json() for pred in self.children ]
        }

class Or(Pred):

    def __init__(self, children):
        for a in children:
          assert isinstance(a,Pred)
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

class True(Pred):

    def to_json(self):
        return { "type": "true" }

class False(Pred):

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
        return self.value

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
        assert type(value) == str
        self.value = value

    def value_to_json(self):
        return self.value

class EthDst(HeaderAndValue):

    def __init__(self, value):
        self.header = "ethdst"
        assert type(value) == str
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
        self.header = "iproto"
        assert type(value) == int and value >= 0
        self.value = value

    def value_to_json(self):
        return self.value

# TODO: import ipaddress to check validity?
class IP4Src(HeaderAndValue):

    def __init__(self, value, mask = None):
        self.header = "ip4src"
        assert type(value) == str
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
        assert type(value) == str
        if mask != None:
            assert(mask) == int
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
        # TODO: Sequence type?
        assert isinstance(children,collections.Iterable) and (isinstance(pol,Policy) for pol in children)
        self.children = children

    def to_json(self):
        return {
          "type": "union",
          "pols": [ pol.to_json() for pol in self.children ]
        }

class Seq(Policy):

    def __init__(self, children):
        assert isinstance(children, collections.Iterable) and (isinstance(pol,Policy) for pol in children)
        self.children = children

    def to_json(self):
        return {
          "type": "seq",
          "pols": [ pol.to_json() for pol in self.children ]
        }

# Shorthands

true = True()
false = False()
id = Filter(true)
drop = Filter(false)


