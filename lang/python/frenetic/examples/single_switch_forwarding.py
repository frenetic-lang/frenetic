import frenetic
from frenetic.syntax import *

def policy(ps):
    vs = 0
    for src in range(1,ps+1):
        f = Filter(Test(Location(Physical(src))))
        dst = filter(lambda x: x != src, range(1,ps+1))
        mods = Union(Mod(Location(Physical(p))) for p in dst)
        both = f >> mods
        if vs == 0:
            vs = both
        else:
            vs = vs | both
    return vs
                     
                     
