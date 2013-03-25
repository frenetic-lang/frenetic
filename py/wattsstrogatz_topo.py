################################################################################
# The Frenetic Project                                                         #
# frenetic@frenetic-lang.org                                                   #
################################################################################
# Licensed to the Frenetic Project by one or more contributors. See the        #
# NOTICE file distributed with this work for additional information            #
# regarding copyright and ownership. The Frenetic Project licenses this        #
# file to you under the following license.                                     #
#                                                                              #
# Redistribution and use in source and binary forms, with or without           #
# modification, are permitted provided the following conditions are met:       #
# - Redistributions of source code must retain the above copyright             #
#   notice, this list of conditions and the following disclaimer.              #
# - Redistributions in binary form must reproduce the above copyright          #
#   notice, this list of conditions and the following disclaimer in            #
#   the documentation or other materials provided with the distribution.       #
# - The names of the copyright holds and contributors may not be used to       #
#   endorse or promote products derived from this work without specific        #
#   prior written permission.                                                  #
#                                                                              #
# Unless required by applicable law or agreed to in writing, software          #
# distributed under the License is distributed on an "AS IS" BASIS, WITHOUT    #
# WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the     #
# LICENSE file distributed with this work for specific language governing      #
# permissions and limitations under the License.                               #
################################################################################
# /updates/examples/routing_topo.py                                            #
# Shortest-path routing topology                                               #
# $Id$ #
################################################################################

from mininet.topo import Topo, Node

class WattsStrogatzTopology(Topo):

    def __init__(self):

        super(WattsStrogatzTopology, self).__init__()

        # add switches
        numSwitches = 6
        numHosts = numSwitches
        hosts = range(1, numHosts+1)
        firstSwitch = max(101, numHosts+1)
        switches = range(firstSwitch, numSwitches + firstSwitch)

        # Add switches
        for s in switches:
            self.add_node(s, Node(is_switch=True))
            
        # Add hosts
        for h in hosts:
            self.add_node(h, Node(is_switch=False))

        # Add links
        for h in hosts:
            self.add_edge(h, switches[h-1])

        rev_switches = list(switches)
        rev_switches.reverse()
        [last] = rev_switches[-1:]
        for s in rev_switches:
            self.add_edge(s, last)
            last = s


        # Add "magic" links

        self.add_edge(101, 103)
        self.add_edge(102, 105)
        
        # Add monitoring host
        self.add_node(99, Node(is_switch=False))

        for s in switches:
            self.add_edge(s, 99)
        
        self.enable_all()
                

Topology = WattsStrogatzTopology
topos = { 'wattsstrogatz_topo': ( lambda: Topology() ) }
