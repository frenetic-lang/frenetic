import frenetic
from frenetic.syntax import *

def policy(ps):
    return Union(Mod(Location(Physical(p))) for p in range(1,ps+1))
                     
                     
