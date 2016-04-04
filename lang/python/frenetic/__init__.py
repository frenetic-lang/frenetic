import uuid, sys, json, base64, time, array, binascii
from datetime import timedelta
from functools import partial
from tornado import httpclient
from tornado.httpclient import AsyncHTTPClient, HTTPRequest
from tornado.ioloop import IOLoop
from frenetic.syntax import PacketIn, PacketOut
from tornado.concurrent import return_future
from tornado import gen
from ryu.lib.packet import *

AsyncHTTPClient.configure("tornado.curl_httpclient.CurlAsyncHTTPClient")

class App(object):

    """This method can be overridden by the application. By default, it simply
       prints the event."""
    def switch_up(self,switch_id,ports):
        print "switch_up(switch_id=%s)" % switch_id

    """This method can be overridden by the application. By default, it simply
       prints the event."""
    def switch_down(self,switch_id):
        print "switch_down(switch_id=%s)" % switch_id

    """This method can be overridden by the application. By default, it simply
       prints the event."""
    def port_up(self,switch_id, port_id):
        print "port_up(switch_id=%s, port_id=%d)" % (switch_id, port_id)

    """This method can be overridden by the application. By default, it simply
       prints the event."""
    def port_down(self,switch_id, port_id):
        print "port_down(switch_id=%s, port_id=%d)" % (switch_id, port_id)

    """This method can be overridden by the application. By default, it simply
       prints the event and drops the packet."""
    def packet_in(self, switch_id, port_id, payload):
        print "packet_in(switch_id=%s, port_id=%d)" % (switch_id, port_id)
        self.pkt_out(switch_id, payload, [])

    def connected(self):
        print "established connection to Frenetic controller"

    def packet(self, payload, protocol):
        pkt = packet.Packet(array.array('b', payload.data))
        for p in pkt:
            if p.protocol_name == protocol:
                return p
        return None

    def pkt_out(self, switch_id, payload, actions, in_port=None):
        msg = PacketOut(switch=switch_id,
                        payload=payload,
                        actions=actions,
                        in_port=in_port)
        pkt_out_url = "http://%s:%s/pkt_out" % (self.frenetic_http_host, self.frenetic_http_port)
        request = HTTPRequest(pkt_out_url, method='POST', body=json.dumps(msg.to_json()))
        return self.__http_client.fetch(request)

    def config(self, compiler_options):
        config_url = "http://%s:%s/config" % (self.frenetic_http_host, self.frenetic_http_port)
        request = HTTPRequest(config_url, method='POST', body=json.dumps(compiler_options.to_json()))
        return self.__http_client.fetch(request)

    def run_response(self, ftr, callback):
      response = ftr.result()
      if(hasattr(response, 'buffer')):
        data = json.loads(response.buffer.getvalue())
        ps = int(data['packets'])
        bs = int(data['bytes'])
        callback([ps, bs])

    @return_future
    def query_helper(self, ftr, callback):
      f = partial(self.run_response, callback=callback)
      IOLoop.instance().add_future(ftr, f)

    def query(self, label):
        url = "http://%s:%s/query/%s" % (self.frenetic_http_host, self.frenetic_http_port, label)
        request = HTTPRequest(url, method='GET', request_timeout=0)
        response_future = self.__http_client.fetch(request)
        return self.query_helper(response_future)

    def run_port_stats(self, ftr, callback):
      response = ftr.result()
      if(hasattr(response, 'buffer')):
        data = json.loads(response.buffer.getvalue())
        dataPrime = {}
        for key in data:
          dataPrime[key] = int(data[key])
        callback(data)

    @return_future
    def port_stats_helper(self, ftr, callback):
      f = partial(self.run_port_stats, callback=callback)
      IOLoop.instance().add_future(ftr, f)

    # Returns a Future where the Result is a dictionary with the
    # following values: port_no, rx_packets, tx_packets, rx_bytes, tx_bytes
    # rx_dropped, tx_dropped, rx_errors, tx_errors, rx_frame_error, rx_over_err,
    # rx_crc_err, collisions. All of these values map to an integer
    def port_stats(self, switch_id, port_id):
      url = "http://%s:%s/port_stats/%s/%s" % (self.frenetic_http_host, self.frenetic_http_port, switch_id, port_id)
      request = HTTPRequest(url, method='GET', request_timeout=0)
      response_future = self.__http_client.fetch(request)
      return self.port_stats_helper(response_future)

    @gen.coroutine
    def current_switches(self):
      url = "http://%s:%s/current_switches" % (self.frenetic_http_host, self.frenetic_http_port)
      req = HTTPRequest(url, method="GET", request_timeout=0)
      resp = yield self.__http_client.fetch(req)
      ret = dict((x["switch_id"], x["ports"]) for x in json.loads(resp.body))
      raise gen.Return(ret)

    def update(self, policy):
        pol_json = json.dumps(policy.to_json())
        url = "http://%s:%s/%s/update_json" % (self.frenetic_http_host, self.frenetic_http_port, self.client_id)
        request = HTTPRequest(url,method='POST',body=pol_json)
        return self.__http_client.fetch(request)

    def __init__(self):
        if not hasattr(self, 'client_id'):
            self.client_id = uuid.uuid4().hex
            print "No client_id specified. Using %s" % self.client_id
        if not hasattr(self, 'frenetic_http_host'):
            self.frenetic_http_host = "localhost"
        if not hasattr(self, 'frenetic_http_port'):
            self.frenetic_http_port = "9000"
        self.__http_client = AsyncHTTPClient()
        self.__connect()

    def __connect(self):
        url = "http://%s:%s/version" % (self.frenetic_http_host, self.frenetic_http_port)
        req = HTTPRequest(url, method='GET',request_timeout=0)
        resp_fut = self.__http_client.fetch(req)
        IOLoop.instance().add_future(resp_fut, self.__handle_connect)

    def __handle_connect(self, response_future):
        try:
            response = response_future.result()
            self.__poll_event()
            self.connected()
        except httpclient.HTTPError as e:
            if e.code == 599:
                print "Frenetic not running, re-trying...."
                one_second = timedelta(seconds = 1)
                IOLoop.instance().add_timeout(one_second, self.__connect)
            else:
                raise e

    def start_event_loop(self):
        print "Starting the tornado event loop (does not return)."
        IOLoop.instance().start()

    def __poll_event(self):
        url = "http://%s:%s/%s/event" % (self.frenetic_http_host, self.frenetic_http_port, self.client_id)
        req = HTTPRequest(url, method='GET',request_timeout=0)
        resp_fut = self.__http_client.fetch(req)

        IOLoop.instance().add_future(resp_fut, self.__handle_event)

    def __handle_event(self, response):
        try: 
            event =  json.loads(response.result().body)
 
            typ = event['type']
            if typ == 'switch_up':
                switch_id = event['switch_id']
                ports = event['ports']
                self.switch_up(switch_id, ports)
            elif typ == 'switch_down':
                switch_id = event['switch_id']
                self.switch_down(switch_id)
            elif typ == 'port_up':
                switch_id = event['switch_id']
                port_id = event['port_id']
                self.port_up(switch_id, port_id)
            elif typ == 'port_down':
                switch_id = event['switch_id']
                port_id = event['port_id']
                self.port_down(switch_id, port_id)
            elif typ == 'packet_in':
                pk = PacketIn(event)
                self.packet_in(pk.switch_id, pk.port_id, pk.payload)
            else:
                print response

            self.__poll_event()

        except httpclient.HTTPError as e:
            if e.code == 599:
                print time.strftime("%c") + " Frenetic crashed, re-trying in 5 seconds...."
                five_seconds = timedelta(seconds = 5)
                # We wait for a connect instead of going through the loop again.
                IOLoop.instance().add_timeout(five_seconds,self.__connect)
            else:
                raise e

 
