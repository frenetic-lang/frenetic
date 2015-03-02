import frenetic
from frenetic.syntax import *

def policy(ps):
    vs = 0
    for dst in range(1,ps+1):
        ip4 = '10.0.0.' + str(dst)
        f = Filter(Test(IP4Dst(ip4)))
        both = f >> Mod(Location(Physical(dst)))
        if vs == 0:
            vs = both
        else:
            vs = vs | both
    return vs
