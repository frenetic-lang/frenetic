from network import *

#for k in [4,6,8,10,12,14,18,20,24,28,34,40,50,58,64]:
for k in [22,26,30,32,36,42,44,46,48]:
    ralg = "spf,allsp"
    print "k=", k
    network("fattree,"+str(k), ralg, None)
    #network("abfattree,"+str(k), ralg, None)
