"""Class to represent results produced by Python applications"""

import json
import base64

class Result:
    def __init__(self):
        pass
    
    def dumps(self):
        pass

class PolicyResult(Result):
    def __init__(self,policy):
        self.policy = policy

    def dumps(self):
        dict = { 'type' : 'policy', 
                 'data' : repr(self.policy) }
        return json.dumps(dict)

class PacketResult(Result):
    def __init__(self,switch,port,packet):
        self.switch = switch
        self.port = port
        self.packet = packet

    def dumps(self):
        dict = { 'type' : 'packet_out',
                 'data' : { 'switch_id' : repr(self.switch),
                            'port_id' : repr(self.port),
                            'packet' : base64.b64encode(self.packet),
                            'actions' : [] }}
        return json.dumps(dict)
    
        
