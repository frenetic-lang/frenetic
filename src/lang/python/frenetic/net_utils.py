# net_utils
# static utliity functions for network-y calculations like subnet masking.

import array
from ryu.lib.packet import packet

class NetUtils():

  @staticmethod
  # Stolen from RYU.  This is part of the RyuApp class, so we can't use it directly.
  def ipv4_to_str(integre):
    ip_list = [str((integre >> (24 - (n * 8)) & 255)) for n in range(4)]
    return '.'.join(ip_list)

  @staticmethod
  def ipv4_to_int(string):
    ip = string.split('.')
    assert len(ip) == 4
    i = 0
    for b in ip:
      b = int(b)
      i = (i << 8) | b
    return i

  @staticmethod
  def net_mask(net_mask_combo):
    # The net is assumed of the form xx.xx.xx.xx/mask using the CIDR notation, as per IP custom
    (net, mask) = net_mask_combo.split("/")
    return (net, int(mask))

  # Given a network and a host, construct an IP.  A lot of the path mapping
  # is done this way, so host x.100 on the real network gets mapped to the bogus addresses 
  # x1.100, x2.100 and x3.100 for each of the paths
  @staticmethod
  def ip_for_network(net, host):
    (net, mask) = NetUtils.net_mask(net)
    # Most of the time, the net is in the proper form with zeroes in the right places, but we 
    # run it through a subnet filter just in case it isn't.
    net_int = NetUtils.ipv4_to_int(net)
    # A mask of all ones is just 2^(n+1) -1
    all_ones = pow(2, mask+1) -1  
    net_int = net_int & (all_ones << (32-mask))
    ip_int = net_int | int(host)
    return NetUtils.ipv4_to_str(ip_int)

  # Given a network and an IP, extract just the host number.
  @staticmethod
  def host_of_ip(src_ip, net):
    (net, mask) = NetUtils.net_mask(net)
    src_ip_int = NetUtils.ipv4_to_int(src_ip)
    net_int = NetUtils.ipv4_to_int(net)
    # A mask of all ones is just 2^(n+1) -1.  The host mask is just the inverse of the net mask
    all_ones = pow(2, mask+1) -1
    host_int = ~ (all_ones << (32-mask))
    return host_int & int(src_ip_int)

  # Given a network and an IP, is the IP in the network?
  @staticmethod
  def ip_in_network(src_ip, net):
    (net, mask) = NetUtils.net_mask(net)
    net_int = NetUtils.ipv4_to_int(net)
    net_mask = (pow(2, mask+1) -1) << (32-mask)
    net_int = net_int & net_mask   # Most likely, there is no change here
    src_net_int = NetUtils.ipv4_to_int(src_ip) & net_mask
    return net_int == src_net_int