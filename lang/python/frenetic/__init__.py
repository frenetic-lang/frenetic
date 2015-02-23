import uuid, sys, json, base64
from tornado.httpclient import AsyncHTTPClient, HTTPRequest
from tornado.ioloop import IOLoop
from frenetic.syntax import PacketIn, PacketOut

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
       prints the event."""
    def packet_in(self, switch_id, port_id, payload):
        print "packet_in(switch_id=%s, port_id=%d, payload=...)" % (switch_id, port_id)

    def pkt_out(self, switch, payload, actions, in_port=None):
        msg = PacketOut(switch=switch,
                        payload=payload,
                        actions=actions,
                        in_port=in_port)
        request = HTTPRequest("http://localhost:9000/pkt_out",
                              method='POST',
                              body=json.dumps(msg.to_json()))
        return self.__http_client.fetch(request)

    def update(self, policy):
        pol_json = json.dumps(policy.to_json())
        url = "http://localhost:9000/%s/update_json" % self.client_id
        request = HTTPRequest(url,method='POST',body=pol_json)
        return self.__http_client.fetch(request)

    def __init__(self):
        if not hasattr(self, 'client_id'):
            self.client_id = uuid.uuid4().hex
            print "No client_id specified. Using %s" % self.client_id
        self.__http_client = AsyncHTTPClient()
        self.__poll_event()

    def start_event_loop(self):
        print "Starting the tornado event loop (does not return)."
        IOLoop.instance().start()

    def __poll_event(self):
        url = "http://localhost:9000/%s/event" % self.client_id
        req = HTTPRequest(url, method='GET',request_timeout=0)
        resp_fut = self.__http_client.fetch(req)

        IOLoop.instance().add_future(resp_fut, self.__handle_event)

    def __handle_event(self, response):
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
