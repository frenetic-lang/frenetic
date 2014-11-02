import sys
import json
from tornado.httpclient import *
from tornado.concurrent import Future
from tornado import ioloop

import base64

AsyncHTTPClient.configure("tornado.curl_httpclient.CurlAsyncHTTPClient")

async_client = AsyncHTTPClient()
sync_client = AsyncHTTPClient()
    
def pkt_out(switch_id, port_id, packet):
    dict = { 'type' : 'packet_out',
             'data' : { 'switch_id' : repr(switch_id),
                        'port_id' : repr(port_id),
                        'packet' : base64.b64encode(packet),
                        'actions' : [] }}
    request = HTTPRequest("http://localhost:9000/pkt_out", 
                          method='POST', 
                          body=json.dumps(dict))
    response = async_client.fetch(request)
    return

def update(policy):
    dict = { 'type' : 'policy', 
             'data' : repr(policy) }
    request = HTTPRequest("http://localhost:9000/update", 
                          method='POST', 
                          body=json.dumps(dict))
    response = async_client.fetch(request)
    return
    

def event(handler):
    def f(response):
        if response.error:
            print response
        else:
            body = response.body
            handler(json.loads(body))
    request = HTTPRequest("http://localhost:9000/event",
                          method='GET', 
                          request_timeout=float(0))
    response = sync_client.fetch(request, callback=f)
    return

def start():
    ioloop.IOLoop.instance().start()

def periodic(f,n):
    cb = ioloop.PeriodicCallback(f,n * 1000)
    cb.start()
    return

def event_loop(handler):
    def handler_(event) :
        handler(event)        
        ioloop.IOLoop.instance().add_callback(loop)
    def loop():
        event(handler_)
    loop()
