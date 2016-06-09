# Mercilessly copied from https://github.com/mininet/mininet/blob/master/examples/vlanhost.py
#
from mininet.node import Host

class VLANHost( Host ):
  "Host connected to VLAN interface"

  def config( self, vlan=1, **params ):
    """Configure VLANHost according to (optional) parameters:
       vlan: VLAN ID for default interface"""

    r = super( VLANHost, self ).config( **params )

    intf = self.defaultIntf()
    # remove IP from default, "physical" interface
    self.cmd( 'ifconfig %s inet 0' % intf )
    # create VLAN interface
    self.cmd( 'vconfig add %s %d' % ( intf, vlan ) )
    # assign the host's IP to the VLAN interface
    self.cmd( 'ifconfig %s.%d inet %s' % ( intf, vlan, params['ip'] ) )
    # update the intf name and host's intf map
    newName = '%s.%d' % ( intf, vlan )
    # update the (Mininet) interface to refer to VLAN interface name
    intf.name = newName
    # add VLAN interface to host's name to intf map
    self.nameToIntf[ newName ] = intf

    return r

