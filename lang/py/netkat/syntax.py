""" Representation of NetKAT abstract syntax tree. """

__all__ = [ 'true', 'false', 'test', 'filter', 'modify', 'id', 'drop', 'seq', 'union' ]

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
            args = [p._repr(self.P_AND) for p in self.args]
            if len(args) == 0:
                return "<none>"
            elif len(args) == 1:
                return args[0]
            else:
                strd = " and ".join(args)
                if parent is None or parent == self.P_AND:
                    return strd
                else:
                    return "(%s)" % strd
        elif self._type == self.P_OR:
            args = [p._repr(self.P_OR) for p in self.args]
            if len(args) == 0:
                return "*"
            elif len(args) == 1:
                return args[0]
            else:
                strd = " or ".join()
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
                strd = " | ".join(args)
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
                strd = "; ".join(args)
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
