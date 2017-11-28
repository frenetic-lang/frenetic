import frenetic
from frenetic.syntax import *

def policy(ps):
    vs = 0
    for dst in range(1,ps+1):
        ip4 = '10.0.0.' + str(dst)
        f = Filter(IP4DstEq(ip4))
        both = f >> SetPort(dst)
        if vs == 0:
            vs = both
        else:
            vs = vs | both
    return vs
