import glob
import sys
from topozoo_routing import *

topo_files = glob.glob('topozoo/'+'*.dot')
for topo_file in topo_files:
    print topo_file
    network(topo_file[:-4], 'spf,allsp')
