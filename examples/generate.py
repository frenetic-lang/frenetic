from network import *

for k in [4,6,8,10,12,14,18,20,24,28,34,40,50,58,64]:
    ralg = "spf,allsp"
    if k < 200:
        ralg += ",disjointtrees"
    print "k=", k
    network("fattree,"+str(k), ralg, None)
    network("abfattree,"+str(k), ralg, None)
