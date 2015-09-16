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

class OFPGetConfigReplySerializable(OFPGetConfigReply):
  def _serialize_body(self):
    msg_pack_into(ofproto.OFP_SWITCH_CONFIG_PACK_STR, self.buf,
      ofproto.OFP_HEADER_SIZE, self.flags, self.miss_send_len)

class OFPPacketInSerializable(OFPPacketIn):
  def _serialize_body(self):
    msg_pack_into(ofproto.OFP_PACKET_IN_PACK_STR, self.buf,
      ofproto.OFP_HEADER_SIZE, self.buffer_id, len(self.data), self.reason,
      self.table_id, self.cookie)
    # Fill in match parameters
    offset = ofproto.OFP_HEADER_SIZE + 16
    self.match.serialize(self.buf, offset)
    # Fill in 2 bytes of padding and  data
    self.buf += "\000\000" + self.data

class OFPPortStatusSerializable(OFPPortStatus):
  def _serialize_body(self):
    port = self.desc
    msg_pack_into(ofproto.OFP_PORT_STATUS_PACK_STR, self.buf,
      ofproto.OFP_HEADER_SIZE, self.reason, 
      port.port_no, port.hw_addr , port.name, port.config , port.state , port.curr,
      port.advertised , port.supported , port.peer , port.curr_speed, port.max_speed
    )

class OFPQueueGetConfigReplySerializable(OFPQueueGetConfigReply):
  def _serialize_body(self):
    msg_pack_into(ofproto.OFP_QUEUE_GET_CONFIG_REPLY_PACK_STR, self.buf,
      ofproto.OFP_HEADER_SIZE, self.port)
    offset = ofproto.OFP_HEADER_SIZE + 8 # 4 for port, 4 for padding
    for queue in self.queues:
      start_of_queue = offset
      msg_pack_into(ofproto.OFP_PACKET_QUEUE_PACK_STR, self.buf,
        offset, queue.queue_id, queue.port, 0)
      offset += OFP_PACKET_QUEUE_SIZE
      for prop in queue.properties:
        # We cheat, since all the properties have the same shape
        msg_pack_into(ofproto.OFP_QUEUE_PROP_HEADER_PACK_STR, self.buf,
          offset, prop.property, ofproto.OFP_QUEUE_PROP_MIN_RATE_SIZE)
        offset += ofproto.OFP_QUEUE_PROP_HEADER_SIZE
        msg_pack_into(ofproto.OFP_QUEUE_PROP_MIN_RATE_PACK_STR, self.buf,
          offset, prop.rate)
        offset += ofproto.OFP_QUEUE_PROP_MIN_RATE_SIZE - 8  # I have no idea why I have to do this.  
      msg_pack_into(ofproto.OFP_PACKET_QUEUE_PACK_STR, self.buf,
        start_of_queue, queue.queue_id, queue.port, offset - start_of_queue)


class OFPRoleReplySerializable(OFPRoleReply):
  def _serialize_body(self):
    # OFP_ROLE_REQUEST_PACK_STR is used because the packets have the same shape
    msg_pack_into(ofproto.OFP_ROLE_REQUEST_PACK_STR, self.buf,
      ofproto.OFP_HEADER_SIZE, self.role, self.generation_id)

class OFPGetAsyncReplySerializable(OFPGetAsyncReply):
  def _serialize_body(self):
    msg_pack_into(ofproto.OFP_ASYNC_CONFIG_PACK_STR, self.buf,
      ofproto.OFP_HEADER_SIZE,
      self.packet_in_mask[0], self.packet_in_mask[1],
      self.port_status_mask[0], self.port_status_mask[1],
      self.flow_removed_mask[0], self.flow_removed_mask[1])  

####################################################################################
# Sample Data

# Temproarily set at not-quite-all-f's because Frenetic doesn't handle all f's
simple_match = OFPMatch(in_port=1, eth_dst='00:ff:ff:ff:ff:ff')

pipeline_match = OFPMatch(in_port = 1, in_phy_port = 2, metadata = (3, 0xff), tunnel_id = 4)

nasty_match = OFPMatch(
  in_port = 1,
  in_phy_port = 2,
  metadata = (3, 0xff),
  eth_dst = 4,
  eth_src = 5,
  eth_type = 6,
  vlan_vid = 7, 
  vlan_pcp = 8, 
  ip_dscp = 9, 
  ip_ecn = 10, 
  ip_proto = 11,
  ipv4_src = 12,
  ipv4_dst = 13,
  tcp_src = 14,
  tcp_dst = 15, 
  udp_src = 16,
  udp_dst = 17,
  sctp_src = 18,
  sctp_dst = 19,
  icmpv4_type = 20,
  icmpv4_code = 21, 
  arp_op = 22,
  arp_spa = 23,
  arp_tpa = 24,
  arp_sha = 25,
  arp_tha = 26,
  ipv6_src = 27,
  ipv6_dst = 28,
  ipv6_flabel = 29,
  icmpv6_type = 30, 
  icmpv6_code = 31,
  ipv6_nd_target = 32,
  ipv6_nd_sll = 33,
  ipv6_nd_tll = 34,
  mpls_label = 35,
  mpls_tc = 36,
  mpls_bos = 1, 
  pbb_isid = 38, 
  tunnel_id = 39,
  ipv6_exthdr = 40
)

one_action = [ OFPActionOutput(666, 0) ]

lotsa_actions = [ 
  OFPActionOutput(983745, 0),
  OFPActionOutput(OFPP_IN_PORT, 0),
  OFPActionOutput(OFPP_TABLE, 0),
  OFPActionOutput(OFPP_NORMAL, 0),
  OFPActionOutput(OFPP_FLOOD, 0),
  OFPActionOutput(OFPP_ALL, 0),
  OFPActionOutput(OFPP_CONTROLLER, 6),
  OFPActionOutput(OFPP_LOCAL, 0),
  OFPActionOutput(OFPP_ANY, 0),
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
  OFPActionSetField(eth_src="00:00:00:00:00:15")  # 21 = 0x15
]

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

msgs["OfpGetConfigReply"] = OFPGetConfigReplySerializable(Datapath, 
  flags=OFPC_FRAG_DROP | OFPC_FRAG_REASM,
  miss_send_len = 603
)

####################################################################################
# OFPT_SET_CONFIG

msgs["OfpSetConfig"] = OFPSetConfig(Datapath, 
  flags=OFPC_FRAG_NORMAL,
  miss_send_len = 603
)

####################################################################################
# OFPT_PACKET_IN

msgs["OfpPacketInBuffered"] = OFPPacketInSerializable(Datapath,
  buffer_id = 2348957,
  reason = OFPR_INVALID_TTL,
  cookie = 0,
  table_id = 100,
  match = pipeline_match,
  data = "Hi mom!  This is a buffered packet in."
)

msgs["OfpPacketInUnbuffered"] = OFPPacketInSerializable(Datapath,
   buffer_id = OFP_NO_BUFFER,
   reason = OFPR_ACTION,
   cookie = 98374,
   table_id = 200,
   match = pipeline_match,
   data = "Hi mom!  This is an unbuffered packet in."
)

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

msgs["OfpPacketOutBuffered"] = OFPPacketOut(Datapath, 
  buffer_id = 81349218,
  in_port = 987245, 
  actions = one_action
)

msgs["OfpPacketOutUnbuffered"] = OFPPacketOut(Datapath, 
  buffer_id = OFP_NO_BUFFER,
  in_port = 987145, 
  actions = lotsa_actions
)

####################################################################################
# OFPT_FLOW_MOD

msgs["OfPFlowModAddSingleAction"] = OFPFlowMod(Datapath,
  cookie = 0x12754879,
  table_id = 100,
  command = OFPFC_ADD,
  idle_timeout = 0x0190,
  hard_timeout = 0x0600,
  priority = 0x5678,
  buffer_id = OFP_NO_BUFFER,
  flags = OFPFF_SEND_FLOW_REM | OFPFF_NO_PKT_COUNTS,
  match = simple_match,
  instructions = [ OFPInstructionActions(OFPIT_APPLY_ACTIONS, one_action) ]
)

msgs["OfPFlowModAddMultiAction"] = OFPFlowMod(Datapath,
  cookie = 0x12554879,
  table_id = 200,
  command = OFPFC_ADD,
  idle_timeout = 0,
  hard_timeout = 0,
  priority = 0x5478,
  buffer_id = 0x87132,
  flags = OFPFF_CHECK_OVERLAP | OFPFF_NO_BYT_COUNTS,
  match = nasty_match,
  instructions = [ OFPInstructionActions(OFPIT_APPLY_ACTIONS, lotsa_actions) ]
)

msgs["OfPFlowModModify"] = OFPFlowMod(Datapath,
  cookie = 0x12753838,
  cookie_mask = 0xffffffff,
  table_id = 200,
  command = OFPFC_MODIFY_STRICT,
  idle_timeout = 0x0191,
  hard_timeout = 0x0601,
  priority = 0x5678,
  buffer_id = OFP_NO_BUFFER,
  flags = OFPFF_RESET_COUNTS,
  match = OFPMatch(tcp_src=8000),
  instructions = [ 
    OFPInstructionGotoTable(table_id=200),
    OFPInstructionWriteMetadata(metadata=2134987, metadata_mask=0xffffffff),
    OFPInstructionActions(OFPIT_WRITE_ACTIONS, one_action),
    OFPInstructionActions(OFPIT_CLEAR_ACTIONS, []),
    OFPInstructionMeter(meter_id = 271)
  ]
)

msgs["OfPFlowModDelete"] = OFPFlowMod(Datapath,
  table_id = OFPTT_ALL,
  command = OFPFC_DELETE,
  flags = OFPFF_RESET_COUNTS,
  priority = 0,
  out_port = 0x921474,
  out_group = OFPG_ANY,
  match = OFPMatch(udp_src=800),
  instructions = [ ]
)

####################################################################################
# OFPT_GROUP_MOD

msgs["OfpGroupModAddNoActions"] = OFPGroupMod(Datapath, 
  command = OFPGC_ADD,
  type_ = OFPGT_ALL, 
  group_id = 391247,
  buckets = []
)

buckets = [ OFPBucket(actions=one_action) ]
msgs["OfpGroupModAddOneAction"] = OFPGroupMod(Datapath, 
  command = OFPGC_ADD,
  type_ = OFPGT_INDIRECT, 
  group_id = 321347,
  buckets = buckets
)

buckets = [ 
  OFPBucket(actions=one_action, weight=40), 
  OFPBucket(actions=one_action, weight=10) 
]
msgs["OfpGroupModAddSelect"] = OFPGroupMod(Datapath, 
  command = OFPGC_ADD,
  type_ = OFPGT_SELECT, 
  group_id = 121347,
  buckets = buckets
)

broadcast_actions = [ OFPActionOutput(p, 0) for p in range(1,4) ]
buckets = [ OFPBucket(actions=[a]) for a in broadcast_actions ]
msgs["OfpGroupModAddAll"] = OFPGroupMod(Datapath, 
  command = OFPGC_ADD,
  type_ = OFPGT_ALL, 
  group_id = 121340,
  buckets = buckets
)

buckets = [ OFPBucket(actions=[a], watch_port=(i+17), watch_group=(int(i/2))) for i, a in enumerate(broadcast_actions) ]
msgs["OfpGroupModAddFf"] = OFPGroupMod(Datapath, 
  command = OFPGC_ADD,
  type_ = OFPGT_FF, 
  group_id = 205793,
  buckets = buckets
)

buckets = [ 
  OFPBucket(actions=one_action, weight=10), 
  OFPBucket(actions=one_action, weight=40) 
]
msgs["OfpGroupModModify"] = OFPGroupMod(Datapath, 
  command = OFPGC_MODIFY,
  type_ = OFPGT_SELECT, 
  group_id = 121347,
  buckets = buckets
)

msgs["OfpGroupModDelete"] = OFPGroupMod(Datapath, 
  command = OFPGC_DELETE,
  type_ = OFPGT_ALL, 
  group_id = 391247,
  buckets = []
)

####################################################################################
# OFPT_PORT_MOD

msgs["OfpPortMod"] = OFPPortMod(Datapath,
  port_no = 77,
  hw_addr = "10:20:30:40:50:60",
  config =   OFPPC_PORT_DOWN | OFPPC_NO_FWD, 
  mask = 0xff,
  advertise = OFPPF_10MB_FD | OFPPF_40GB_FD | OFPPF_FIBER
)

####################################################################################
# OFPT_TABLE_MOD

msgs["OfpTableMod"] = OFPTableMod(Datapath,
  table_id = 156,
  config = 3 
)

####################################################################################
# OFPT_MULTIPART_REQUEST

msgs["OfpDescStatsRequest"] = OFPDescStatsRequest(Datapath, flags=0)
# These are awaiting an OfpMatch
#msgs["OfpFlowStatsRequest"] = OFPFlowStatsRequest(Datapath, 
#  flags = 1,
#  table_id = 13241234,
#
#)
#msgs["OfpAggregateStatsRequest"] = OFPAggregateStatsRequest(Datapath, 
msgs["OfpTableStatsRequest"] = OFPTableStatsRequest(Datapath, flags=0)
msgs["OfpPortStatsRequest"] = OFPPortStatsRequest(Datapath, flags=0, port_no=555)
msgs["OfpQueueStatsRequest"] = OFPQueueStatsRequest(Datapath, flags=0, port_no=565, queue_id=192834)
msgs["OfpGroupStatsRequest"] = OFPGroupStatsRequest(Datapath, flags=0, group_id=5123456)
msgs["OfpGroupDescStatsRequest"] = OFPGroupDescStatsRequest(Datapath, flags=0)
msgs["OfpGroupFeaturesStatsRequest"] = OFPGroupFeaturesStatsRequest(Datapath, flags=0)
msgs["OfpMeterStatsRequest"] = OFPMeterStatsRequest(Datapath, flags=0, meter_id=6234324)
msgs["OfpMeterConfigStatsRequest"] = OFPMeterConfigStatsRequest(Datapath, flags=0, meter_id=6234324)
# msgs["OfpMeterFeaturesStatsRequest"] = OFPMeterFeaturesStatsRequest(Datapath, flags=0)
# tbs = [
#   OFPTableFeatures(table_id=100, name="ACL Table", metadata_match=0xfffff, metadata_write=0xff, 
#     config=0, max_entries=500, properties=),

# ]
# msgs["OfpTableFeaturesStatsRequest"] = OFPTableFeaturesStatsRequest(Datapath, 
#   body= 
#   flags=0
# )
msgs["OfpPortDescStatsRequest"] = OFPPortDescStatsRequest(Datapath, flags=0)


####################################################################################
# OFPT_BARRIER_REQUEST

msgs["OfpBarrierRequest"] = OFPBarrierRequest(Datapath)

####################################################################################
# OFPT_BARRIER_REPLY

msgs["OfpBarrierReply"] = OFPBarrierReply(Datapath)

####################################################################################
# OFPT_QUEUE_GET_CONFIG_REQUEST

msgs["OfpQueueGetConfigRequest"] = OFPQueueGetConfigRequest(Datapath, 
  port= 2387456
)

####################################################################################
# OFPT_QUEUE_GET_CONFIG_REPLY

msgs["OfpQueueGetConfigReply"] = OFPQueueGetConfigReplySerializable(Datapath, 
  port=2387456,
  queues = [
    OFPPacketQueue(queue_id=2134, port=2387456, properties=[
      OFPQueuePropMinRate(rate=20), OFPQueuePropMaxRate(rate=46),
    ]),
    OFPPacketQueue(queue_id=284570349, port=2387456, properties=[
      OFPQueuePropMinRate(rate=33), OFPQueuePropMaxRate(rate=0xffff),
    ])
  ]
)

####################################################################################
# OFPT_ROLE_REQUEST

msgs["OfpRoleRequest"] = OFPRoleRequest(Datapath, 
  role = OFPCR_ROLE_EQUAL,
  generation_id = 92580291354
)

####################################################################################
# OFPT_ROLE_REPLY

msgs["OfpRoleReply"] = OFPRoleReplySerializable(Datapath, 
  role = OFPCR_ROLE_SLAVE,
  generation_id = 92581791354
)

####################################################################################
# OFPT_GET_ASYNC_REQUEST

msgs["OfpGetAsyncRequest"] = OFPGetAsyncRequest(Datapath)

####################################################################################
# OFPT_GET_ASYNC_REPLY

msgs["OfpGetAsyncReply"] = OFPGetAsyncReplySerializable(Datapath,
  packet_in_mask = [ 1 << OFPR_NO_MATCH, 1 << OFPR_ACTION | 1 << OFPR_INVALID_TTL ],
  port_status_mask = [ 1 << OFPPR_ADD | 1 << OFPPR_MODIFY , 1 << OFPPR_DELETE ],
  flow_removed_mask = [ 1 << OFPRR_IDLE_TIMEOUT | 1 << OFPRR_DELETE, 1 << OFPRR_HARD_TIMEOUT | 1 << OFPRR_GROUP_DELETE ]
)

####################################################################################
# OFPT_SET_ASYNC

msgs["OfpSetAsync"] = OFPSetAsync(Datapath,
  packet_in_mask = [ 1 << OFPR_ACTION | 1 << OFPR_INVALID_TTL, 1 << OFPR_NO_MATCH ],
  port_status_mask = [  1 << OFPPR_DELETE, 1 << OFPPR_ADD | 1 << OFPPR_MODIFY ],
  flow_removed_mask = [ 1 << OFPRR_HARD_TIMEOUT | 1 << OFPRR_GROUP_DELETE, 1 << OFPRR_IDLE_TIMEOUT | 1 << OFPRR_DELETE ]
)


####################################################################################
# Finally, dump them all out.

for msg_name, msg in msgs.iteritems():
  msg.serialize()
  hex_file_name = 'data/openflow0x04/{0}.hex'.format(msg_name)
  with open(hex_file_name, 'w') as f:
    f.write( binascii.hexlify(msg.buf) )
