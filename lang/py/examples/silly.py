from ryu.lib.packet import packet
import base64
from netkat import webkat
from netkat.syntax import *
from netkat.result import *

"""Silly repeater"""

def state():
    pass

##
# Main handler
##
def handler(event):
    print event
    typ = event['type']
    if typ == 'packet_in':
        sw = event['switch_id']
        pt = event['port_id']
        bits = base64.b64decode(event['payload']['buffer'])
        webkat.pkt_out(sw,pt,bits)
    else:
        return None

def main():
    webkat.update(modify("port", "http"))
    while True:
        handler(webkat.event())

if __name__ == '__main__':
    main()
