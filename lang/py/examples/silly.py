
from ryu.lib.packet import packet
import base64
import netkat.flaskapp
from netkat.syntax import *
from netkat.result import *

"""Silly repeater"""

def state():
    pass

##
# Main handler
##
def handler(_, event):
    print event
    typ = event['type']
    if typ  == 'switch_up':
        return PolicyResult(modify("port", "python"))
    if typ == 'packet_in':
        sw = event['switch_id']
        pt = event['port_id']
        bits = base64.b64decode(event['payload']['buffer'])
        return PacketResult(sw, pt, bits)
    else:
        return None

if __name__ == '__main__':
    app = netkat.flaskapp.create(state, handler)
    app.debug = True
    app.run()
