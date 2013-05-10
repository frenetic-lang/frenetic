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

from mininet.topo import Topo

class DiamondTopology(Topo):

    def __init__(self):

        super(DiamondTopology, self).__init__()

        # add switches
        switches = ["s1", "s2", "s3", "s4"]
        hosts = ["h1","h2"]

        # Add switches
        for s in switches:
            self.addSwitch(s)
            
        # Add hosts
        for h in hosts:
            self.addHost(h)

        # Add host links
        self.addLink("h1", "s1", port2=1)
        self.addLink("h2", "s4", port2=1)

        # Add switch links
        self.addLink("s1","s2", 2, 1)
        self.addLink("s1","s3", 3, 1)
        self.addLink("s2","s3", 2, 2)        
        self.addLink("s2","s4", 3, 2)
        self.addLink("s3","s4", 3, 3)

Topology = DiamondTopology
topos = { 'diamond_topo': ( lambda: Topology() ) }
