""" Representation of NetKAT abstract syntax tree. """
import base64

__all__ = [ 'true', 'false', 'test', 'filter', 'modify', 'id', 'drop', 'seq', 'union' ]

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
        self.data = data

    def to_json(self):
        return { "type": "notbuffered", "data": base64.b64encode(self.data) }


class packet_out():

    def __init__(self, switch, payload, actions, in_port = None):
        assert type(switch) == int
        assert in_port == None or type(in_port) == int
        self.switch = switch
        self.payload = payload
        self.actions = actions
        self.in_port = in_port

    def to_json(self):
        return { "switch": self.switch,
                 "in_port": self.in_port,
                 "actions": [ action.to_json() for action in self.actions ],
                 "payload": self.payload.to_json() }

class packet_in():

    def __init__(self, json):
        assert (json['type'] == 'packet_in')
        self.switch_id = json['switch_id']
        self.port_id = json['port_id']
        self.payload = payload.from_json(json['payload'])

class Pred:
    """ A class to represent NetKAT predicates. This class's constructor should
    not be called directly. Instead, the user should construct objects using
    the various convenience functions defined in this module, together with the
    class's overloaded operators such as `&`, `|`, and `~` for conjunction,
    disjunction, and negation, respectively. """

    P_TEST = 1
    P_AND  = 2
    P_OR   = 3
    P_NEG  = 4

    P_TRUE = 5
    P_FALS = 6

    def __init__(self, _type, *args):
        self._type = _type
        self.args = args

    def _is_atomic(self):
        return self._type not in [self.P_AND, self.P_OR, self.P_NEG]

    def _is_basic(self):
        return self._type not in [self.P_AND, self.P_OR]

    def __and__(self, other):
        if self._is_basic() and other._is_basic():
            return Pred(self.P_AND, self, other)
        elif self._type == other._type and self._type == self.P_AND:
            return Pred(self.P_AND, *(self.args + other.args))
        else:
            return Pred(self.P_AND, self, other)

    def __or__(self, other):
        if self._is_basic() and other._is_basic():
            return Pred(self.P_OR, self, other)
        elif self._type == other._type and self._type == self.P_OR:
            return Pred(self.P_OR, *(self.args + other.args))
        else:
            return Pred(self.P_OR, self, other)

    def __invert__(self):
        return Pred(self.P_NEG, self)

    def _repr(self, parent=None):
        if self._type == self.P_TRUE:
            return "*"
        elif self._type == self.P_FALS:
            return "<none>"
        elif self._type == self.P_TEST:
            return "%s = %s" % self.args
        elif self._type == self.P_NEG:
            return "not %s" % self.args[0]._repr(self.P_NEG)
        elif self._type == self.P_AND:
            if len(self.args) == 0:
                strd = "<none>"
            else:
                strd = " and ".join([p._repr(self.P_AND) for p in self.args])

            if parent is None or parent == self.P_AND:
                return strd
            else:
                return "(%s)" % strd
        elif self._type == self.P_OR:
            if len(self.args) == 0:
                strd = "*"
            else:
                strd = " or ".join([p._repr(self.P_OR) for p in self.args])

            if parent is None or parent == self.P_OR:
                return strd
            else:
                return "(%s)" % strd

    def __repr__(self):
        return self._repr()

def true():
    """ The true Pred """
    return Pred(Pred.P_TRUE)

def false():
    """ The false Pred """
    return Pred(Pred.P_FALS)

def test(field, value):
    """ A field test Pred. The field and the values will be cast to a string.  """
    return Pred(Pred.P_TEST, field, value)

class Policy:
    P_FILTER = 1
    P_MOD    = 2

    P_UNION  = 3
    P_SEQ    = 4
    P_STAR   = 5

    def __init__(self, _type, *args):
        self._type = _type
        self.args = args

    def _is_basic(self):
        return self._type not in [self.P_FILTER, self.P_MOD]

    def __rshift__(self, other):
        if self._is_basic() and other._is_basic():
            return Policy(self.P_SEQ, self, other)
        elif self._type == other._type and self._type == self.P_SEQ:
            return Policy(self.P_SEQ, *(self.args + self.other))
        else:
            return Policy(self.P_SEQ, self, other)

    def __or__(self, other):
        if self._is_basic() and other._is_basic():
            return Policy(self.P_UNION, self, other)
        elif self._type == other._type and self._type == self.P_UNION:
            return Policy(self.P_UNION, *(self.args + self.other))
        else:
            return Policy(self.P_UNION, self, other)

    def _repr(self, parent=None):
        if self._type == self.P_FILTER:
            return "filter %s" % repr(self.args[0])
        elif self._type == self.P_MOD:
            return "%s := %s" % self.args
        elif self._type == self.P_UNION:
            if len(self.args) == 0:
                strd = "drop"
            else:
                strd = " | ".join([p._repr(self.P_UNION) for p in self.args])

            if parent is None or parent == self.P_UNION:
                return strd
            else:
                return "(%s)" % strd
        elif self._type == self.P_SEQ:
            if len(self.args) == 0:
                strd = "filter *"
            else:
                strd = "; ".join([p._repr(self.P_SEQ) for p in self.args])

            if parent is None or parent == self.P_SEQ:
                return strd
            else:
                return "(%s)" % strd
        elif self._type == self.P_STAR:
            return "(%s)*" % repr(self.args[0])

    def __repr__(self):
        return self._repr()

def filter(pred):
    """ A filter Policy based on a Pred """
    return Policy(Policy.P_FILTER, pred)

def modify(field, value):
    """ A field modification Policy. The field and the values will be cast to a
    string. """
    return Policy(Policy.P_MOD, field, value)

def id():
    """ The identity Policy leaves packets as they are. """
    return filter(true())

def drop():
    """ The drop Policy does not allow any packets to pass. """
    return filter(false())

def seq(pols):
    """ Sequentially compose two Policies. """
    return Policy(Policy.P_SEQ, *pols)

def union(pols):
    """ Parallel compose two Policies. """
    return Policy(Policy.P_UNION, *pols)
