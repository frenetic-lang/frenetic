import json
from tornado.httpclient import *
from tornado.concurrent import Future
from tornado import ioloop

import base64

AsyncHTTPClient.configure("tornado.curl_httpclient.CurlAsyncHTTPClient")

client = AsyncHTTPClient()
    
def pkt_out(switch_id, port_id, packet):
    dict = { 'type' : 'packet_out',
             'data' : { 'switch_id' : repr(switch_id),
                        'port_id' : repr(port_id),
                        'packet' : base64.b64encode(packet),
                        'actions' : [] }}
    request = HTTPRequest("http://localhost:9000/pkt_out", 
                          method='POST', 
                          body=json.dumps(dict))
    response = client.fetch(request)
    return

def update(policy):
    dict = { 'type' : 'policy', 
             'data' : repr(policy) }
    request = HTTPRequest("http://localhost:9000/update", 
                          method='POST', 
                          body=json.dumps(dict))
    response = client.fetch(request)
    return
    

def event(handler):
    print "EVENT"
    def f(response):
        if response.error:
            handler(None)
        else:
            body = response.body
            handler(json.loads(body))
    response = client.fetch("http://localhost:9000/event", callback=f)
    return

def start():
    ioloop.IOLoop.instance().start()
