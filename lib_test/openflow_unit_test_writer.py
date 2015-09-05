# Openflow Unit Test Writer
#
# Uses RYU to construct test request/reply messages of all types and shapes.  Then:
# serializes each message to data/openflow0x04/OfpMessageType.hex in hex format
# 
# You should only have to run this if the RYU library changes - e.g. if there are bugs
# in its OpenFlow parser that get fixed.
#
# Craig Riecke, Programmer/Analyst CoSciN
# September, 2015

from ryu.ofproto.ofproto_v1_3_parser import *
from ryu.ofproto.ofproto_v1_3 import *
from ryu.ofproto import ofproto_v1_3, ofproto_v1_3_parser
import binascii, re

class Datapath(object):
  ofproto = ofproto_v1_3
  ofproto_parser = ofproto_v1_3_parser

msgs = {}

openflow_constants = dir(ofproto_v1_3)

# These classes extend the base RYU classes to serialize incoming messages, which is something
# they really don't have to do in a production setting (RYU doesn't need to send a GET_CONFIG_REPLY,
# for example, because it's not a switch.)  We don't handle the full generality of
# each message, just enough to write a test packet.

class OFPHelloSerializable(OFPHello):
  def _serialize_body(self):
    assert self.elements is not None
    # We are just doing one version bitmap in this incarnation.
    assert max(self.elements.versions) < 32

    msg_pack_into(ofproto.OFP_HELLO_ELEM_VERSIONBITMAP_HEADER_PACK_STR, self.buf,
      ofproto.OFP_HEADER_SIZE, ofproto.OFPHET_VERSIONBITMAP, 8)

    # Create a bitmap where bit n means version n
    bitmap = 0
    for v in self.elements.versions:
      bitmap |= (1<<v)
    msg_pack_into('!I', self.buf, ofproto.OFP_HEADER_SIZE+OFP_HELLO_ELEM_VERSIONBITMAP_HEADER_SIZE, bitmap)

class OFPSwitchFeaturesSerializable(OFPSwitchFeatures):
  def _serialize_body(self):
    msg_pack_into(ofproto.OFP_SWITCH_FEATURES_PACK_STR, self.buf,
      ofproto.OFP_HEADER_SIZE, self.datapath_id, self.n_buffers, self.n_tables,
      self.auxiliary_id, self.capabilities, 0)

class OFPPacketInSerializable(OFPPacketIn):
  def _serialize_body(self):
    msg_pack_into(ofproto.OFP_PACKET_IN_PACK_STR, self.buf,
      ofproto.OFP_HEADER_SIZE, self.buffer_id, self.total_len, self.reason,
      self.table_id, self.cookie)

class OFPPortStatusSerializable(OFPPortStatus):
  def _serialize_body(self):
    port = self.desc
    msg_pack_into(ofproto.OFP_PORT_STATUS_PACK_STR, self.buf,
      ofproto.OFP_HEADER_SIZE, self.reason, 
      port.port_no, port.hw_addr , port.name, port.config , port.state , port.curr,
      port.advertised , port.supported , port.peer , port.curr_speed, port.max_speed
    )


####################################################################################
# OFPT_HELLO

hevb = OFPHelloElemVersionBitmap([1,4])  # This is Openflow 1.0 = 0x01 and 1.3 = 0x04
msgs["OfpHello"] = OFPHelloSerializable(Datapath, elements=hevb)

####################################################################################
# OFPT_ERROR Error Messages

# We could introspect these types, but there's no good way to match them with their codes
error_types = {
  "OFPET_HELLO_FAILED": ("OFPHFC","Hello"),
  "OFPET_BAD_REQUEST": ("OFPBRC","Req"),
  "OFPET_BAD_ACTION": ("OFPBAC","Act"),
  "OFPET_BAD_INSTRUCTION": ("OFPBIC","Inst"),
  "OFPET_BAD_MATCH": ("OFPBAD","Mat"),
  "OFPET_FLOW_MOD_FAILED": ("OFPFMFC","Fl"),
  "OFPET_GROUP_MOD_FAILED": ("OFPGMFC","Gr"),
  "OFPET_PORT_MOD_FAILED": ("OFPPMFC","Po"),
  "OFPET_TABLE_MOD_FAILED": ("OFPTMFC","Ta"),
  "OFPET_QUEUE_OP_FAILED": ("OFPQOFC","Qu"),
  "OFPET_SWITCH_CONFIG_FAILED": ("OFPSCFC","Sc"),
  "OFPET_ROLE_REQUEST_FAILED": ("OFPRRFC","Ro"),
  "OFPET_METER_MOD_FAILED": ("OFPMMFC","Me"),
  "OFPET_TABLE_FEATURES_FAILED": ("OFPTFFC","Tf"),
 }
for error_type, (error_code_prefix,fr_prefix) in error_types.iteritems():
  error_codes = [cn for cn in openflow_constants if re.match(error_code_prefix + "_", cn)]
  for error_code in error_codes:
    msg_name = 'OfpErrorMsg_{0}_{1}'.format(error_type, error_code)  
    msgs[msg_name] = OFPErrorMsg(
      Datapath, 
      type_=getattr(ofproto_v1_3,error_type), 
      code=getattr(ofproto_v1_3,error_code), 
      data=msg_name
    )
    # This approximates the Frenetic analogues, but it's not perfect
    #camel_case_error_type = error_type.replace("OFPET_","").title().replace("_","")
    #camel_case_error_code = error_code.replace(error_code_prefix,fr_prefix).title().replace("_","")
    #print '    ("{0}", {1} {2});'.format(msg_name,camel_case_error_type,camel_case_error_code)

####################################################################################
# OFPT_ECHO_REQUEST 

msgs["OfpEchoRequest"] = OFPEchoRequest(Datapath, data="OfpEchoRequest")

####################################################################################
# OFPT_ECHO_REPLY

msgs["OfpEchoReply"] = OFPEchoReply(Datapath, data="OfpEchoReply")

####################################################################################
# OFPT_FEATURES_REQUEST

msgs["OfpFeaturesRequest"] = OFPFeaturesRequest(Datapath)

####################################################################################
# OFPT_FEATURES_REPLY

msgs["OfpFeaturesReply"] = OFPSwitchFeaturesSerializable(Datapath,
  datapath_id = 9210263729383,
  n_buffers = 897345987,
  n_tables = 250,
  auxiliary_id = 65,
  capabilities = OFPC_FLOW_STATS | OFPC_GROUP_STATS | OFPC_PORT_BLOCKED
)

####################################################################################
# OFPT_GET_CONFIG_REQUEST

msgs["OfpGetConfigRequest"] = OFPGetConfigRequest(Datapath)

####################################################################################
# OFPT_GET_CONFIG_REPLY

msgs["OfpGetConfigReply"] = OFPGetConfigReply(Datapath, 
  flags=OFPC_FRAG_NORMAL | OFPC_FRAG_REASM,
  miss_send_len = 603
)

####################################################################################
# OFPT_SET_CONFIG

msgs["OfpSetConfig"] = OFPSetConfig(Datapath, 
  flags=OFPC_FRAG_NORMAL & OFPC_FRAG_REASM,
  miss_send_len = 603
)

####################################################################################
# OFPT_PACKET_IN
# TODO: WAITING FOR A good implementation of OFP_MATCH structure

# msgs["OfpPacketInBuffered"] = OFPPacketInSerializable(Datapath,
#   datapath_id = 4350263729384,
#   buffer_id = 2348957,
#   total_len = ,
#   reason = OFPR_INVALID_TTL,
#   table_id = 100,
#   cookie = 9837450982734345234
# )

# msgs["OfpPacketInUnbuffered"] = OFPPacketInSerializable(Datapath,
#   datapath_id = 4350263529384,
#   buffer_id = OFP_NO_BUFFER,
#   total_len = ,
#   reason = OFPR_ACTION,
#   table_id = 100,
#   cookie = 9837450986734345234
# )

####################################################################################
# OFPT_FLOW_REMOVED
# Same as OFPT_PACKET_IN

####################################################################################
# OFPT_PORT_STATUS

port = OFPPort(
  77,  # port_no
  "\x10\x20\x30\x40\x50\x60", # hw_addr
  "Port 77", # name,
  OFPPC_PORT_DOWN | OFPPC_NO_FWD, # config
  OFPPS_BLOCKED | OFPPS_LIVE, # state
  OFPPF_10MB_HD | OFPPF_10GB_FD | OFPPF_COPPER, # curr
  OFPPF_10MB_FD | OFPPF_40GB_FD | OFPPF_FIBER, # advertised
  OFPPF_100MB_HD | OFPPF_100GB_FD | OFPPF_AUTONEG, # supported
  OFPPF_1GB_HD | OFPPF_1TB_FD | OFPPF_PAUSE, # peer
  10000000, # curr_speed
  100000000 # max_speed
)
msgs["OfpPortStatus"] = OFPPortStatusSerializable(Datapath,
  reason = OFPPR_MODIFY,
  desc = port
)

####################################################################################
# OFPT_PACKET_OUT

actions = [ 
  OFPActionOutput(666)
]
msgs["OfpPacketOutBuffered"] = OFPPacketOut(Datapath, 
  buffer_id = 81349218,
  in_port = 987245, 
  actions = actions
)

actions = [ 
  OFPActionOutput(983745, 0),
  OFPActionOutput(OFPP_IN_PORT, 1),
  OFPActionOutput(OFPP_TABLE, 2),
  OFPActionOutput(OFPP_NORMAL, 3),
  OFPActionOutput(OFPP_FLOOD, 4),
  OFPActionOutput(OFPP_ALL, 5),
  OFPActionOutput(OFPP_CONTROLLER, 6),
  OFPActionOutput(OFPP_LOCAL, 7),
  OFPActionOutput(OFPP_ANY, 8),
  OFPActionGroup(9),
  OFPActionSetQueue(10),
  OFPActionSetMplsTtl(11),
  OFPActionDecMplsTtl(),
  OFPActionSetNwTtl(13),
  OFPActionDecNwTtl(),
  OFPActionCopyTtlOut(),
  OFPActionCopyTtlIn(),
  OFPActionPushVlan(17),
  OFPActionPushMpls(18),
  OFPActionPopVlan(),
  OFPActionPopMpls(20),
  # We test all the set field possibilities in OFPT_FLOW_MOD
  OFPActionSetField(eth_src="00:00:00:00:00:21")
]
msgs["OfpPacketOutUnbuffered"] = OFPPacketOut(Datapath, 
  buffer_id = OFP_NO_BUFFER,
  in_port = 987145, 
  actions = actions
)

####################################################################################
# Finally, dump them all out.

for msg_name, msg in msgs.iteritems():
  msg.serialize()
  hex_file_name = 'data/openflow0x04/{0}.hex'.format(msg_name)
  with open(hex_file_name, 'w') as f:
    f.write( binascii.hexlify(msg.buf) )
