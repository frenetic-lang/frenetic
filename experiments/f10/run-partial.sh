#!/bin/bash
probnetkat.dump examples/output/abfattree_4_sw_20 -1 1/128,1/64,1/32,1/16,1/8,1/4,1/2
probnetkat.dump examples/output/abfattree_4_sw_20 -1 1/128,1/64,1/32,1/16,1/8,1/4,1/2 -hopcount
probnetkat.dump examples/output/fattree_4_sw_20 -1 1/128,1/64,1/32,1/16,1/8,1/4,1/2
probnetkat.dump examples/output/fattree_4_sw_20 -1 1/128,1/64,1/32,1/16,1/8,1/4,1/2 -hopcount
