#!/us/bin/env python
with open("abilene.txt") as f:
  l = f.readlines()[0].split()
  for s in range(12):
    for d in range(12):
      if s != d:
        print "h" + str(s+1) + " h" + str(d+1) + " " + str(int(round(float(l[s*12+d])*8/100/100000)))

  
