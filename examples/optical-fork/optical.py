"""
Classes and functions to integrate LincSwitch with Mininet, especially the
optical extensions. This is based on code from the ONOS project which can be
found at:

https://github.com/opennetworkinglab/onos/blob/master/tools/test/topos/opticalUtils.py
"""

import re
import json
import os.path
from time import sleep

from mininet.net import Mininet
from mininet.node import Switch
from mininet.link import Link, Intf
from mininet.util import quietRun, _colonHex
from mininet.log import info, error

SLEEP_TIME = 2
TIMEOUT = 60

LINC_ROOT = os.path.expanduser("~/Public/LINC-Switch")
CONFIG_GEN_ROOT = os.path.expanduser("~/Public/LINC-config-generator/")

LINC_CONFIG = os.path.join(LINC_ROOT, "rel/linc/releases/1.0/sys.config")
LINC_BIN = os.path.join(LINC_ROOT, "rel/linc/bin/linc")

def format_dpid(dpid):
    dpid_hex = int('0x'+dpid, 16)
    return _colonHex(dpid_hex, 8)

class OpticalSwitch(Switch):
    """Generic superclass for optical switches."""
    pass

class OpticalIntf(Intf):
    """Generic superclass for optical switches."""
    pass

class OpticalLink(Link):
    """Generic superclass for optical switches."""
    pass

class LINCIntf(OpticalIntf):
    """Dummy class to maintain Mininet compatibility"""

    def __init__(self):
        pass
    def delete(self):
        pass

class LINCSwitch(OpticalSwitch):
    """Wraps and provides an interface to a Linc-Switch.

    This is mostly a set of utility functions for reading and generating the
    sys.config files for a Linc-Swith instance and presenting that information
    via Pythonic APIs.
    """

    def __init__(self, name, dpid=None, config_path=None,
                 controller=None, **params):
        """Initialize this LINC-Switch.

        Most of the parameters will be passed on to the standard Mininet Link
        constructor, except for the LINC-specific attributes, such as config
        filepath.
        """
        params['inNamespace'] = False
        Switch.__init__(self, name, dpid=dpid, **params)
        self.config_path = LINC_CONFIG if config_path is None else config_path
        self.controller = controller

    def start(self, controllers):
        pass

    def linc_json(self):
        """Return a JSON object in the LINC configuration generator format"""
        return {"nodeDpid" : format_dpid(self.dpid)}


class LINCLink(Link):
    """ Class representing a link where at least one endpoint is a LINC switch
    """

    def __init__(self, node1, node2, port1=None, port2=None,
                 intfName1=None, intfName2=None, **params):
        """Creates a dummy link without a virtual ethernet pair.

        If both endpoints are LINC switches, there is no need to deal with any
        interfaces. If one endpoint is a Mininet switch, then the tap interface
        corresponding to the other endpoint must be added to the Mininet
        switch. But since the names of the taps are not known until *after* the
        whole network's JSON has been converted to the LINC format, the
        tap is not added by this constructor. Instead, the corresponding port
        information is added here which is then used by the LincNet's start()
        method (further down in this file).

        """
        self.node1 = node1
        self.node2 = node2
        self.port1 = node1.newPort() if port1 is None else port1
        self.port2 = node2.newPort() if port2 is None else port2

        if isinstance(node1, LINCSwitch) and isinstance(node2, LINCSwitch):
            self.cross_connect = False
        else:
            self.cross_connect = True

        if not isinstance(node1, LINCSwitch):
            self.tap_node = node1
            self.optical_port = port2
            self.optical_node = node2
        elif not isinstance(node2, LINCSwitch):
            self.tap_node = node2
            self.optical_port = port1
            self.optical_node = node1
        else:
            self.optical_node = None
            self.optical_port = None
            self.tap_node = None

        self.intf1 = LINCIntf()
        self.intf2 = LINCIntf()

    @classmethod
    def makeIntfPair(cls, intfName1, intfName2, *args, **kwargs):
        pass

    def linc_json(self):
        """Return a dictionary representing a JSON object in the LINC
        configuration generator format.
        """
        json = {}
        json["nodeDpid1"] = format_dpid(self.node1.dpid)
        json["nodeDpid2"] = format_dpid(self.node2.dpid)
        json["params"] = {"port1" : self.port1,
                          "port2" : self.port2}
        json["type"] = "pktOptLink" if self.cross_connect else "wdmLink"
        return json


class LINCNet(Mininet):
    """Reprsent and manage a network containing LINC-Switches"""
    def __init__(self, config_path=None, **params):
        self.config_path = LINC_CONFIG if config_path is None else config_path
        Mininet.__init__(self, **params)

    @staticmethod
    def setup_taps(intfs):
        """Add taps and bring them up. """
        for i in intfs:
            quietRun('ip tuntap add dev %s mode tap' % i)
            quietRun('ip link set dev %s up' % i)
            info('*** Intf %s set\n' % i)


    def taps(self):
        """Return list of all the taps in sys.config. """
        fd = open(self.config_path, 'r', 0)
        sys_data = fd.read()
        taps = re.findall('tap\d+', sys_data)
        fd.close()
        return taps

    def find_tap(self, node, port):
        """Parse a sys.config file to find the tap interface for a port."""
        switch = False
        port_line = ''
        intf_lines = []

        with open(self.config_path) as f:
            for line in f:
                if 'tap' in line:
                    intf_lines.append(line)
                if node.dpid in line.translate(None, ':'):
                    switch = True
                    continue
                if switch:
                    if 'switch' in line:
                        switch = False
                    if 'port_no,%s' % port in line:
                        port_line = line
                        break

        if port_line:
            m = re.search('port,\d+', port_line)
            port = m.group(0).split(',')[1]
        else:
            error('***ERROR: Could not find any ports in sys.config\n')
            return

        for intf_line in intf_lines:
            if 'port,%s' % port in intf_line:
                return re.findall('tap\d+', intf_line)[0]

    def wait(self, timeout=TIMEOUT):
        """Wait until all tap interfaces are available"""
        tap_count = 0
        time = 0
        for link in self.links:
            if isinstance(link, LINCLink) and link.cross_connect:
                tap_count += 1

        while True:
            if str(tap_count) == quietRun('ip addr | grep tap | wc -l',
                                         shell=True).strip('\n'):
                return True
            if timeout:
                if time >= TIMEOUT:
                    error('***ERROR: LINC OE did not start within %s seconds\n'
                          % TIMEOUT)
                    return False
                time += SLEEP_TIME
            sleep(SLEEP_TIME)

    def linc_json(self):
        """Create a dictionary providing a JSON representation for this network
        in the LINC-OE config generator format
        """
        switches = []
        links = []
        for switch in self.switches:
            if isinstance(switch, LINCSwitch):
                switches.append({"nodeDpid" : format_dpid(switch.dpid)})

        for link in self.links:
            if isinstance(link, LINCLink):
                links.append(link.linc_json())

        return {"switchConfig" : switches,
                "linkConfig" : links}

    def cross_connect(self):
        """Attach tap devices exposed by the optical switches as part of a
        cross-connect link to the Mininet switches on the other end of the
        corresponding link.
        """
        for link in self.links:
            if isinstance(link, LINCLink) and link.cross_connect:
                link.tap_node.attach(
                    self.find_tap(link.optical_node, link.optical_port))

    def start(self):
        """Start this network topology.

        The following steps are taken to start this network:
          1. Start the Mininet network.
          2. Create a LINC config file for the optical components via a JSON
             dump
          3. Create and bring up the tap devices exposed by the LINC switches
          4. Start the LINC network using the above generated config.
          5. Add the tap devices to the Mininet switches on the other side of
             cross-connect links.
        """
        Mininet.start(self)

        info("Writing JSON for Linc topology")
        with open("topology.json", 'w') as outfile:
            json.dump(self.linc_json(), outfile, indent=4,
                      separators=(',', ': '))

        info("Creating LincSwitch config")

        output = quietRun(
            '%s/config_generator topology.json %s/priv/sys.config.template %s %s'
            % (CONFIG_GEN_ROOT, CONFIG_GEN_ROOT,
               self.controllers[0].ip, self.controllers[0].port), shell=True)
        if output:
            error('***ERROR: Error creating LincSwitch config file: %s\n'
                  % output)
            return False

        info('Copying sys.config to Linc Switch directory: ', output + '\n')
        output = quietRun('cp -v sys.config %s' % LINC_CONFIG,
                          shell=True).strip('\n')
        info(output + '\n')

        info('Adding and bringing up taps')
        LINCNet.setup_taps(self.taps())

        info('Starting Linc Switch OE')
        output = quietRun('%s start' % LINC_BIN, shell=True)
        if output:
            error('***ERROR: LINC-OE: %s' % output + '\n')
            quietRun('%s stop' % LINC_BIN, shell=True)
            return False

        info('Waiting for Linc Switch to start...\n')
        self.wait()

        info('Adding cross-connect (tap) interfaces to packet switches...\n')
        self.cross_connect()

    def stop(self):
        """Stop this network topology. First stop the Mininet network, then stop
        the LINC network
        """
        Mininet.stop(self)
        quietRun('%s stop' % LINC_BIN, shell=True)
