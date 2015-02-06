""" Representation of NetKAT abstract syntax tree. """
import base64

class action:
    pass

class pseudoPort:
    pass

class physical(pseudoPort):

    def __init__(self, port):
        assert (port >= 1 and port <= 65535)
        self.port = port

    def to_json(self):
        return { "type": "physical", "port": self.port }

class output(action):

    def __init__(self, p):
        assert isinstance(p, pseudoPort)
        self.pseudoPort = p

    def to_json(self):
        return { "type": "output", "pseudoport": self.pseudoPort.to_json() }

class payload:
    pass

    @staticmethod
    def from_json(json):
        # TODO(arjun): fix names on the OCaml side
        if json['id'] == None:
            return notbuffered(base64.b64decode(json['buffer']))
        else:
            # TODO(arjun): may have some bytes. do not discard
            return buffered(json['id'])


class buffered(payload):

    def __init__(self, buffer_id):
        assert type(buffer_id) == int
        self.buffer_id = buffer_id

    def to_json(self):
        return { "type": "buffered", "bufferid": self.buffer_id }

class notbuffered(payload):

    def __init__(self, data):
        assert type(data) == str
        self.data = data

    def to_json(self):
        return { "type": "notbuffered", "data": base64.b64encode(self.data) }


class packet_out():

    def __init__(self, switch, payload, actions, in_port = None):
        assert type(switch) == int
        self.switch = switch
        assert isinstance(payload,buffered) or isinstance(payload,notbuffered)
        self.payload = payload
        assert isinstance(actions,output)
        self.actions = actions
        assert in_port == None or type(in_port) == int
        self.in_port = in_port

    def to_json(self):
        return { "switch": self.switch,
                 "in_port": self.in_port,
                 "actions": [ action.to_json() for action in self.actions ],
                 "payload": self.payload.to_json() }

class packet_in():

    def __init__(self, json):
        assert (json['type'] == 'packet_in')
        assert type(json['switch_id']) == int
        self.switch_id = json['switch_id']
        assert type(json['switch_id']) == int
        self.port_id = json['port_id']
        # TODO: assert isinstance(payload,buffered) or isinstance(payload,notbuffered)
        self.payload = payload.from_json(json['payload'])

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
        self.children = children

    def to_json(self):
        return {
          "type": "and",
          "preds": [ pred.to_json() for pred in self.children ]
        }

class Or(Pred):

    def __init__(self, children):
        self.children = children

    def to_json(self):
        return {
          "type": "or",
          "preds": [ pred.to_json() for pred in self.children ]
        }

class Not(Pred):

    def __init__(self, pred):
        self.children = pred

    def to_json(self):
        return {
          "type": "neg",
          "pred": pred.to_json
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
        self.pred = pred

    def to_json(self):
        return { "type": "filter", "pred": self.pred.to_json() }

class HeaderAndValue(object):
    pass

class Switch(HeaderAndValue):

    def __init__(self, value):
        self.header = "switch"
        assert(type(value) == int)
        self.value = value

    def value_to_json(self):
        return self.value

class Pipe(object):

    def __init__(self, name):
        self.name = name

    def to_json(self):
        return { "type": "pipe", "name": self.name }

class Physical(object):

    def __init__(self, port):
        assert type(port) == int
        self.port = port

    def to_json(self):
        return { "type": "physical", "port": self.port }

class Location(HeaderAndValue):

    def __init__(self, value):
        self.header = "location"
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
        assert type(value) == str
        self.value = value

    def value_to_json(self):
        return self.value

class VlanPcp(HeaderAndValue):

    def __init__(self, value):
        self.header = "vlanpcp"
        assert type(value) == str
        self.value = value

    def value_to_json(self):
        return self.value

class EthType(HeaderAndValue):

    def __init__(self, value):
        self.header = "ethtype"
        assert type(value) == int
        self.value = value

    def value_to_json(self):
        return self.value

class IPProto(HeaderAndValue):

    def __init__(self, value):
        self.header = "iproto"
        assert type(value) == int
        self.value = value
    
    def value_to_json(self):
        return self.value

class IP4Src(HeaderAndValue):

    def __init__(self, value):
        self.header = "ip4src"
        assert type(value) == str
        self.value = value

    def value_to_json(self):
        return self.value

class IP4Dst(HeaderAndValue):

    def __init__(self, value):
        self.header = "ip4dst"
        assert type(value) == str
        self.value = value

    def value_to_json(self):
        return self.value

class TCPSrcPort(HeaderAndValue):

    def __init__(self, value):
        self.header = "tcpsrcport"
        assert type(value) == int
        self.value = value

    def value_to_json(self):
        return self.value

class TCPDstPort(HeaderAndValue):

    def __init__(self, value):
        self.header = "tcpdstport"
        assert type(value) == int
        self.value = value

    def value_to_json(self):
        return self.value

class Mod(Policy):

    def __init__(self, hv):
        self.hv = hv

    def to_json(self):
        return {
          "type": "mod",
          "header": self.hv.header,
          "value": self.hv.value_to_json()
        }

class Union(Policy):

    def __init__(self, children):
        self.children = children

    def to_json(self):
        return {
          "type": "union",
          "pols": [ pol.to_json() for pol in self.children ]
        }

class Seq(Policy):

    def __init__(self, children):
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

