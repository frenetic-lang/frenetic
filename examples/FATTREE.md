k-ary FatTree (k even):
   * # ports = k per switch
   * pods: k, each with 2 layers of k/2 switches, for total of k^2 pod switches
   * core switches: (k/2)^2
   * total # switches = k^2 + (k/2)^2 = 5/4 k^2 
   * total # hosts = 1/4 k^3
   * total # ports = 5/4 k^3
