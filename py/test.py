#!/usr/bin/env python

import sys
from MininetDriver import MininetRunner
from MininetDriver import time
from mininet.topolib import TreeTopo

dir =  "test-" + time()
runner = MininetRunner(TreeTopo(depth=2,fanout=2),dir)
runner.start_controller('arjun', '../ocaml/Main_Verified.d.byte')
print dir
