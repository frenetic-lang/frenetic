# Openflow Unit Test Writer
#
# Uses RYU to construct test request/reply messages of all types and shapes.  Then:
# serializes each message to data/openflow0x04/OfpMessageType.hex in hex format
# 
# You should only have to run this if the RYU library changes 
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
# for example, because it's a controller and not a switch.)  We don't handle the full generality of
# each message, just enough to write a test packet.

class OFPHelloSerializable(OFPHello):
  def _serialize_body(self):
    assert self.elements is not None
    # We are just doing one version bitmap in this incarnation.
    assert max(self.elements.versions) < 32

    msg_pack_into(OFP_HELLO_ELEM_VERSIONBITMAP_HEADER_PACK_STR, self.buf,
      OFP_HEADER_SIZE, OFPHET_VERSIONBITMAP, 8)

    # Create a bitmap where bit n means version n
    bitmap = 0
    for v in self.elements.versions:
      bitmap |= (1<<v)
    msg_pack_into('!I', self.buf, OFP_HEADER_SIZE+OFP_HELLO_ELEM_VERSIONBITMAP_HEADER_SIZE, bitmap)

class OFPSwitchFeaturesSerializable(OFPSwitchFeatures):
  def _serialize_body(self):
    msg_pack_into(OFP_SWITCH_FEATURES_PACK_STR, self.buf,
      OFP_HEADER_SIZE, self.datapath_id, self.n_buffers, self.n_tables,
      self.auxiliary_id, self.capabilities, 0)

class OFPGetConfigReplySerializable(OFPGetConfigReply):
  def _serialize_body(self):
    msg_pack_into(OFP_SWITCH_CONFIG_PACK_STR, self.buf,
      OFP_HEADER_SIZE, self.flags, self.miss_send_len)

class OFPPacketInSerializable(OFPPacketIn):
  def _serialize_body(self):
    msg_pack_into(OFP_PACKET_IN_PACK_STR, self.buf,
      OFP_HEADER_SIZE, self.buffer_id, len(self.data), self.reason,
      self.table_id, self.cookie)
    # Fill in match parameters
    offset = OFP_HEADER_SIZE + 16
    self.match.serialize(self.buf, offset)
    # Fill in 2 bytes of padding and  data
    self.buf += "\000\000" + self.data

class OFPFlowRemovedSerializable(OFPFlowRemoved):
  def _serialize_body(self):
    msg_pack_into(OFP_FLOW_REMOVED_PACK_STR0, self.buf,
      OFP_HEADER_SIZE, self.cookie, self.priority, self.reason,
      self.table_id, self.duration_sec, self.duration_nsec,
      self.idle_timeout, self.hard_timeout, self.packet_count,
      self.byte_count
    )
    # Fill in match parameters
    offset = OFP_HEADER_SIZE + 40
    self.match.serialize(self.buf, offset)

class OFPPortStatusSerializable(OFPPortStatus):
  def _serialize_body(self):
    port = self.desc
    msg_pack_into(OFP_PORT_STATUS_PACK_STR, self.buf,
      OFP_HEADER_SIZE, self.reason, 
      port.port_no, port.hw_addr , port.name, port.config , port.state , port.curr,
      port.advertised , port.supported , port.peer , port.curr_speed, port.max_speed
    )

class OFPQueueGetConfigReplySerializable(OFPQueueGetConfigReply):
  def _serialize_body(self):
    msg_pack_into(OFP_QUEUE_GET_CONFIG_REPLY_PACK_STR, self.buf,
      OFP_HEADER_SIZE, self.port)
    offset = OFP_HEADER_SIZE + 8 # 4 for port, 4 for padding
    for queue in self.queues:
      start_of_queue = offset
      msg_pack_into(OFP_PACKET_QUEUE_PACK_STR, self.buf,
        offset, queue.queue_id, queue.port, 0)
      offset += OFP_PACKET_QUEUE_SIZE
      for prop in queue.properties:
        # We cheat, since all the properties have the same shape
        msg_pack_into(OFP_QUEUE_PROP_HEADER_PACK_STR, self.buf,
          offset, prop.property, OFP_QUEUE_PROP_MIN_RATE_SIZE)
        offset += OFP_QUEUE_PROP_HEADER_SIZE
        msg_pack_into(OFP_QUEUE_PROP_MIN_RATE_PACK_STR, self.buf,
          offset, prop.rate)
        offset += OFP_QUEUE_PROP_MIN_RATE_SIZE - 8  # I have no idea why I have to do this.  
      msg_pack_into(OFP_PACKET_QUEUE_PACK_STR, self.buf,
        start_of_queue, queue.queue_id, queue.port, offset - start_of_queue)

class OFPDescStatsReplySerializable(OFPDescStatsReply):
  def _serialize_body(self):
    msg_pack_into(OFP_MULTIPART_REPLY_PACK_STR, self.buf,
      OFP_HEADER_SIZE, OFPMP_DESC, 0)
    msg = self.body
    msg_pack_into(OFP_DESC_PACK_STR, self.buf,
      OFP_HEADER_SIZE + 8, 
      msg.mfr_desc, msg.hw_desc, msg.sw_desc, msg.serial_num, msg.dp_desc)

class OFPFlowStatsReplySerializable(OFPFlowStatsReply):
  def _serialize_body(self):
    msg_pack_into(OFP_MULTIPART_REPLY_PACK_STR, self.buf,
      OFP_HEADER_SIZE, OFPMP_FLOW, 0)
    flow_stats = self.body
    start_of_reply = offset = OFP_HEADER_SIZE + 8
    msg_pack_into(OFP_FLOW_STATS_0_PACK_STR, self.buf, offset,
      0, flow_stats.table_id, flow_stats.duration_sec, flow_stats.duration_nsec,
      flow_stats.priority, flow_stats.idle_timeout, flow_stats.hard_timeout, flow_stats.flags,
      flow_stats.cookie, flow_stats.packet_count, flow_stats.byte_count
    )
    offset = OFP_HEADER_SIZE + 8 + OFP_FLOW_STATS_0_SIZE
    offset += flow_stats.match.serialize(self.buf, offset)
    for instruction in flow_stats.instructions:
      instruction.serialize(self.buf, offset)
      offset += instruction.len
    # Backfill length
    msg_pack_into("!H", self.buf, start_of_reply, offset - start_of_reply)

class OFPAggregateStatsReplySerializable(OFPAggregateStatsReply):
  def _serialize_body(self):
    msg_pack_into(OFP_MULTIPART_REPLY_PACK_STR, self.buf,
      OFP_HEADER_SIZE, OFPMP_AGGREGATE, 0)
    offset = OFP_HEADER_SIZE + 8
    aggregate_stats = self.body
    msg_pack_into(OFP_AGGREGATE_STATS_REPLY_PACK_STR, self.buf, offset,
      aggregate_stats.packet_count, aggregate_stats.byte_count, aggregate_stats.flow_count
    )

class OFPTableStatsReplySerializable(OFPTableStatsReply):
  def _serialize_body(self):
    msg_pack_into(OFP_MULTIPART_REPLY_PACK_STR, self.buf,
      OFP_HEADER_SIZE, OFPMP_TABLE, 0)
    offset = OFP_HEADER_SIZE + 8
    table_stats = self.body
    msg_pack_into(OFP_TABLE_STATS_PACK_STR, self.buf, offset,
      table_stats.table_id, table_stats.active_count, table_stats.lookup_count,
      table_stats.matched_count
    )

class OFPPortStatsReplySerializable(OFPPortStatsReply):
  def _serialize_body(self):
    msg_pack_into(OFP_MULTIPART_REPLY_PACK_STR, self.buf,
      OFP_HEADER_SIZE, OFPMP_PORT_STATS, 0)
    offset = OFP_HEADER_SIZE + 8
    port_stats = self.body
    msg_pack_into(OFP_PORT_STATS_PACK_STR, self.buf, offset,
      port_stats.port_no, port_stats.rx_packets, port_stats.tx_packets,
      port_stats.rx_bytes, port_stats.tx_bytes, port_stats.rx_dropped,
      port_stats.tx_dropped, port_stats.rx_errors, port_stats.tx_errors,
      port_stats.rx_frame_err, port_stats.rx_over_err, port_stats.rx_crc_err,
      port_stats.collisions, port_stats.duration_sec, port_stats.duration_nsec
    )

class OFPQueueStatsReplySerializable(OFPQueueStatsReply):
  def _serialize_body(self):
    msg_pack_into(OFP_MULTIPART_REPLY_PACK_STR, self.buf,
      OFP_HEADER_SIZE, OFPMP_QUEUE, 0)
    offset = OFP_HEADER_SIZE + 8
    queue_stats = self.body
    msg_pack_into(OFP_QUEUE_STATS_PACK_STR, self.buf, offset,
      queue_stats.port_no, queue_stats.queue_id,
      queue_stats.tx_bytes, queue_stats.tx_packets, queue_stats.tx_errors,
      queue_stats.duration_sec, queue_stats.duration_nsec
    )

class OFPGroupStatsReplySerializable(OFPGroupStatsReply):
  def _serialize_body(self):
    msg_pack_into(OFP_MULTIPART_REPLY_PACK_STR, self.buf,
      OFP_HEADER_SIZE, OFPMP_GROUP, 0)
    start_of_reply = offset = OFP_HEADER_SIZE + 8
    group_stats = self.body
    msg_pack_into(OFP_GROUP_STATS_PACK_STR, self.buf, offset,
      0, group_stats.group_id, group_stats.ref_count,
      group_stats.packet_count, group_stats.byte_count, group_stats.duration_sec,
      group_stats.duration_nsec
    )
    offset += OFP_GROUP_STATS_SIZE
    for bucket in group_stats.bucket_stats:
      msg_pack_into(OFP_BUCKET_COUNTER_PACK_STR, self.buf, offset,
        bucket.packet_count, bucket.byte_count)
      offset += OFP_BUCKET_COUNTER_SIZE
    # Backfill length
    msg_pack_into("!H", self.buf, start_of_reply, offset - start_of_reply)

class OFPGroupDescStatsReplySerializable(OFPGroupDescStatsReply):
  def _serialize_body(self):
    msg_pack_into(OFP_MULTIPART_REPLY_PACK_STR, self.buf,
      OFP_HEADER_SIZE, OFPMP_GROUP_DESC, 0)
    start_of_reply = offset = OFP_HEADER_SIZE + 8
    group_desc = self.body
    msg_pack_into(OFP_GROUP_DESC_STATS_PACK_STR, self.buf, offset,
      0, group_desc.type, group_desc.group_id
    )
    offset += OFP_GROUP_DESC_STATS_SIZE
    for bucket in group_desc.buckets:
      bucket.serialize(self.buf, offset)
      offset += bucket.len
    # Backfill length
    msg_pack_into("!H", self.buf, start_of_reply, offset - start_of_reply)

class OFPGroupFeaturesStatsReplySerializable(OFPGroupFeaturesStatsReply):
  def _serialize_body(self):
    msg_pack_into(OFP_MULTIPART_REPLY_PACK_STR, self.buf,
      OFP_HEADER_SIZE, OFPMP_GROUP_FEATURES, 0)
    offset = OFP_HEADER_SIZE + 8
    group_features = self.body
    msg_pack_into(OFP_GROUP_FEATURES_PACK_STR, self.buf, offset,
      group_features.types, group_features.capabilities,
      group_features.max_groups[0], group_features.max_groups[1], 
      group_features.max_groups[2], group_features.max_groups[3], 
      group_features.actions[0], group_features.actions[1], 
      group_features.actions[2], group_features.actions[3] 
    )

class OFPMeterStatsReplySerializable(OFPMeterStatsReply):
  def _serialize_body(self):
    msg_pack_into(OFP_MULTIPART_REPLY_PACK_STR, self.buf,
      OFP_HEADER_SIZE, OFPMP_METER, 0)
    start_of_reply = offset = OFP_HEADER_SIZE + 8
    meter_stats = self.body
    msg_pack_into(OFP_METER_STATS_PACK_STR, self.buf, offset,
      meter_stats.meter_id, 0,
      meter_stats.flow_count, meter_stats.packet_in_count,
      meter_stats.byte_in_count, meter_stats.duration_sec,
      meter_stats.duration_nsec
    )
    offset += OFP_METER_STATS_SIZE
    for band in meter_stats.band_stats:
      msg_pack_into(OFP_METER_BAND_STATS_PACK_STR, self.buf, offset,
        band.packet_band_count, band.byte_band_count)
      offset += OFP_BUCKET_COUNTER_SIZE      
    # Backfill length
    msg_pack_into("!IH", self.buf, start_of_reply, meter_stats.meter_id, offset - start_of_reply)

class OFPMeterConfigStatsReplySerializable(OFPMeterConfigStatsReply):
  def _serialize_body(self):
    msg_pack_into(OFP_MULTIPART_REPLY_PACK_STR, self.buf,
      OFP_HEADER_SIZE, OFPMP_METER_CONFIG, 0)
    start_of_reply = offset = OFP_HEADER_SIZE + 8
    meter_config = self.body
    msg_pack_into(OFP_METER_CONFIG_PACK_STR, self.buf, offset,
      0, meter_config.flags, meter_config.meter_id
    )
    # I'm not going to program bands because I'm oh so tired.
    offset += OFP_METER_CONFIG_SIZE
    # Backfill length
    msg_pack_into("!H", self.buf, start_of_reply, offset - start_of_reply)

class OFPMeterFeaturesStatsReplySerializable(OFPMeterFeaturesStatsReply):
  def _serialize_body(self):
    msg_pack_into(OFP_MULTIPART_REPLY_PACK_STR, self.buf,
      OFP_HEADER_SIZE, OFPMP_METER_FEATURES, 0)
    offset = OFP_HEADER_SIZE + 8
    meter_features = self.body
    msg_pack_into(OFP_METER_FEATURES_PACK_STR, self.buf, offset,
      meter_features.max_meter, meter_features.band_types,
      meter_features.capabilities, meter_features.max_bands,
      meter_features.max_color
    )

class OFPTableFeaturesStatsReplySerializable(OFPTableFeaturesStatsReply):
  # Mostly we just rip off the serializers for TableFeatures since they're used on the request
  def _serialize_body(self):
    msg_pack_into(OFP_MULTIPART_REPLY_PACK_STR, self.buf,
      OFP_HEADER_SIZE, OFPMP_TABLE_FEATURES, 0)
    bin_body = bytearray()
    for p in self.body:
        bin_body += p.serialize()
    self.buf += bin_body

class OFPPortDescStatsReplySerializable(OFPPortDescStatsReply):
  # Mostly we just rip off the serializers for PortDesc since they're used on the request
  def _serialize_body(self):
    msg_pack_into(OFP_MULTIPART_REPLY_PACK_STR, self.buf,
      OFP_HEADER_SIZE, OFPMP_PORT_DESC, 0)
    offset = OFP_HEADER_SIZE + 8
    for port in self.body:
      msg_pack_into(OFP_PORT_PACK_STR, self.buf,
        offset,
        port.port_no, port.hw_addr , port.name, port.config , port.state , port.curr,
        port.advertised , port.supported , port.peer , port.curr_speed, port.max_speed
      )
      offset += OFP_PORT_SIZE

class OFPRoleReplySerializable(OFPRoleReply):
  def _serialize_body(self):
    # OFP_ROLE_REQUEST_PACK_STR is used because the packets have the same shape
    msg_pack_into(OFP_ROLE_REQUEST_PACK_STR, self.buf,
      OFP_HEADER_SIZE, self.role, self.generation_id)

class OFPGetAsyncReplySerializable(OFPGetAsyncReply):
  def _serialize_body(self):
    msg_pack_into(OFP_ASYNC_CONFIG_PACK_STR, self.buf,
      OFP_HEADER_SIZE,
      self.packet_in_mask[0], self.packet_in_mask[1],
      self.port_status_mask[0], self.port_status_mask[1],
      self.flow_removed_mask[0], self.flow_removed_mask[1])  

# This class actually implements the OXM header length correctly, which Frenetic does as well

class OFPOxmIdSerializable(OFPOxmId):
 def serialize(self):
    # Note we don't overwrite self.length.  Otherwise, this is just like OFPOxmId.serialize
    (n, _v, _m) = oxm_from_user(self.type, None)
    oxm = (n << (1 + 8)) | (self.hasmask << 8) | self.length
    buf = bytearray()
    msg_pack_into(self._PACK_STR, buf, 0, oxm)
    return buf

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
  ip_ecn = 2,  # This is necesary because it's only two bits 
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

one_table_property = [
  OFPTableFeaturePropInstructions(OFPTFPT_INSTRUCTIONS, [ OFPInstructionId(OFPIT_GOTO_TABLE) ])
]

lotsa_table_properties = [
  OFPTableFeaturePropInstructions(OFPTFPT_INSTRUCTIONS, [ OFPInstructionId(OFPIT_GOTO_TABLE) ]),
  OFPTableFeaturePropInstructions(
    OFPTFPT_INSTRUCTIONS_MISS, 
    [ OFPInstructionId(OFPIT_WRITE_METADATA), OFPInstructionId(OFPIT_WRITE_ACTIONS),
      OFPInstructionId(OFPIT_APPLY_ACTIONS), OFPInstructionId(OFPIT_CLEAR_ACTIONS), 
      OFPInstructionId(OFPIT_METER)
    ]
  ),
  OFPTableFeaturePropNextTables(OFPTFPT_NEXT_TABLES, [100, 150, 200, 250]),
  OFPTableFeaturePropNextTables(OFPTFPT_NEXT_TABLES_MISS, [1, 2, 3, 4]),
  OFPTableFeaturePropActions(OFPTFPT_WRITE_ACTIONS, [ OFPActionId(OFPAT_OUTPUT) ]),
  OFPTableFeaturePropActions(
    OFPTFPT_WRITE_ACTIONS_MISS, 
    [ OFPActionId(OFPAT_COPY_TTL_OUT), OFPActionId(OFPAT_COPY_TTL_IN),  
      OFPActionId(OFPAT_PUSH_VLAN), OFPActionId(OFPAT_POP_VLAN),  
      OFPActionId(OFPAT_PUSH_MPLS), OFPActionId(OFPAT_POP_MPLS),  
    ]
  ),
  OFPTableFeaturePropActions(OFPTFPT_APPLY_ACTIONS, [ OFPActionId(OFPAT_SET_QUEUE) ]),
  OFPTableFeaturePropActions(
    OFPTFPT_APPLY_ACTIONS_MISS, 
    [ OFPActionId(OFPAT_GROUP), OFPActionId(OFPAT_SET_NW_TTL),  
      OFPActionId(OFPAT_DEC_NW_TTL), OFPActionId(OFPAT_SET_FIELD),  
      OFPActionId(OFPAT_PUSH_PBB), OFPActionId(OFPAT_POP_PBB)  
    ]
  ),
  # We don't bother matching all fields here because matches are covered elsewhere
  OFPTableFeaturePropOxm(OFPTFPT_MATCH, [ OFPOxmIdSerializable('tunnel_id', length=8) ]),  
  OFPTableFeaturePropOxm(OFPTFPT_WILDCARDS, [ OFPOxmIdSerializable('vlan_pcp', length=1) ]),  
  OFPTableFeaturePropOxm(OFPTFPT_WRITE_SETFIELD, [ OFPOxmIdSerializable('ipv6_flabel', length=4) ]),  
  OFPTableFeaturePropOxm(OFPTFPT_WRITE_SETFIELD_MISS, [ OFPOxmIdSerializable('metadata', length=8) ]),  
  OFPTableFeaturePropOxm(OFPTFPT_APPLY_SETFIELD, [ OFPOxmIdSerializable('ipv6_nd_target', length=16) ]),  
  OFPTableFeaturePropOxm(OFPTFPT_APPLY_SETFIELD_MISS, [ OFPOxmIdSerializable('eth_dst', length=6) ]),  
]

####################################################################################
# OFPT_HELLO

hevb = OFPHelloElemVersionBitmap([1,4])  # This is Openflow 1.0 = 0x01 and 1.3 = 0x04
msgs["OfpHello"] = OFPHelloSerializable(Datapath, elements=hevb)

####################################################################################
# OFPT_ERROR Error Messages - This is a little different in that we dump all the hex
# to one file OfpErrorMsg.hex

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

with open('data/openflow0x04/OfpErrorMsg.hex', 'w') as f:
  for error_type, (error_code_prefix,fr_prefix) in error_types.iteritems():
    error_codes = [cn for cn in openflow_constants if re.match(error_code_prefix + "_", cn)]
    for error_code in error_codes:
      msg_name = '{0}_{1}'.format(error_type, error_code)  
      msg = OFPErrorMsg(
        Datapath, 
        type_=getattr(ofproto_v1_3,error_type), 
        code=getattr(ofproto_v1_3,error_code), 
        data=msg_name
      )
      msg.serialize()
      f.write( msg_name + "," + binascii.hexlify(msg.buf) + "\n" )

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

msgs["OfpFlowRemoved"] = OFPFlowRemovedSerializable(Datapath,
   cookie = 98374,
   priority = 8977,
   reason = OFPRR_HARD_TIMEOUT,
   table_id = 200,
   duration_sec = 8127346,
   duration_nsec = 1213414,
   idle_timeout = 999,
   hard_timeout = 9999,
   packet_count = 872364012876751,
   byte_count = 198237501837540,
   match = nasty_match
)


####################################################################################
# OFPT_PORT_STATUS

ports = OFPPort(
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
  desc = ports
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

msgs["OfpFlowStatsRequest"] = OFPFlowStatsRequest(Datapath, 
 flags = 1,  # A "more" bit set just to make sure it works OK.  We're not going to send > 1
 table_id = 199,
 out_port = 12325,
 out_group = 9712346,
 cookie= 871625978634,
 cookie_mask = 0xffffffffffff,
 match = simple_match
)

msgs["OfpAggregateStatsRequest"] = OFPAggregateStatsRequest(Datapath, 
 flags = 0,
 table_id = 201,
 out_port = 12325,
 out_group = 9712346,
 cookie= 871625978634,
 cookie_mask = 0xffffffffffff,
 match = simple_match
)

msgs["OfpTableStatsRequest"] = OFPTableStatsRequest(Datapath, flags=0)

msgs["OfpPortStatsRequest"] = OFPPortStatsRequest(Datapath, flags=0, port_no=555)

msgs["OfpQueueStatsRequest"] = OFPQueueStatsRequest(Datapath, flags=0, port_no=565, queue_id=192834)

msgs["OfpGroupStatsRequest"] = OFPGroupStatsRequest(Datapath, flags=0, group_id=5123456)

msgs["OfpGroupDescStatsRequest"] = OFPGroupDescStatsRequest(Datapath, flags=0)

msgs["OfpGroupFeaturesStatsRequest"] = OFPGroupFeaturesStatsRequest(Datapath, flags=0)

msgs["OfpMeterStatsRequest"] = OFPMeterStatsRequest(Datapath, flags=0, meter_id=6234324)

msgs["OfpMeterConfigStatsRequest"] = OFPMeterConfigStatsRequest(Datapath, flags=0, meter_id=6234324)

msgs["OfpMeterFeaturesStatsRequest"] = OFPMeterFeaturesStatsRequest(Datapath, flags=0)

tbs = [
  OFPTableFeaturesStats(
    table_id=1, 
    name="Init Table", 
    metadata_match=0, 
    metadata_write=0, 
    config=3, # OFPTC_DEPRECATED_MASK, 
    max_entries=10, 
    properties=one_table_property 
  ),
  OFPTableFeaturesStats(
    table_id=100, 
    name="ACL Table", 
    metadata_match=0xfffff, 
    metadata_write=0xff, 
    config=3, # OFPTC_DEPRECATED_MASK, 
    max_entries=500, 
    properties=lotsa_table_properties 
  ),
]

msgs["OfpTableFeaturesStatsRequest"] = OFPTableFeaturesStatsRequest(Datapath, 
  flags = 0,
  body = tbs  
)

msgs["OfpPortDescStatsRequest"] = OFPPortDescStatsRequest(Datapath, flags=0)

####################################################################################
# OFPT_MULTIPART_REPLY

msgs["OfpDescStatsReply"] = OFPDescStatsReplySerializable(Datapath, 
  body = OFPDescStats(
    mfr_desc = "Manufacturer Description",
    hw_desc = "Hardware Description",
    sw_desc = "Software Descriptiuon",
    serial_num = "0123456789-JHJH",
    dp_desc = "Dataplane Description"
  )
)

msgs["OfpFlowStatsReply"] = OFPFlowStatsReplySerializable(Datapath, 
  body = OFPFlowStats(
    table_id = 100,
    duration_sec = 999,
    duration_nsec = 888,
    priority = 0x5678,
    idle_timeout = 0x0190,
    hard_timeout = 0x0600,
    flags = OFPFF_SEND_FLOW_REM | OFPFF_NO_PKT_COUNTS,
    cookie = 0x12754879,
    packet_count = 4000,
    byte_count = 3000,
    match = simple_match,
    instructions = [ OFPInstructionActions(OFPIT_APPLY_ACTIONS, one_action) ]
  )
)

msgs["OfpAggregateStatsReply"] = OFPAggregateStatsReplySerializable(Datapath, 
  body = OFPAggregateStats(
    packet_count = 4000,
    byte_count = 3000,
    flow_count = 2000
  )
)

msgs["OfpTableStatsReply"] = OFPTableStatsReplySerializable(Datapath, 
  body = OFPTableStats(
    table_id = 100,
    active_count = 600,
    lookup_count = 2000,
    matched_count = 666
  )
)

msgs["OfpPortStatsReply"] = OFPPortStatsReplySerializable(Datapath, 
  body = OFPPortStats(
    port_no = 574190793,
    rx_packets = 1113204397,
    tx_packets = 2702231185,
    rx_bytes = 2451900840,
    tx_bytes = 2654217578,
    rx_dropped = 2311349152,
    tx_dropped = 2340791430,
    rx_errors = 1441457975,
    tx_errors = 3861416712,
    rx_frame_err = 3760794366,
    rx_over_err = 3471122481,
    rx_crc_err = 38255885,
    collisions = 4183796980,
    duration_sec = 327091,
    duration_nsec = 417782
  )
)

msgs["OfpQueueStatsReply"] = OFPQueueStatsReplySerializable(Datapath, 
  body = OFPQueueStats(
    port_no = 574190793,
    queue_id = 98734,
    tx_bytes = 2654217578,
    tx_packets = 2702231185,
    tx_errors = 3861416712,
    duration_sec = 327091,
    duration_nsec = 417782
  )
)

msgs["OfpGroupStatsReply"] = OFPGroupStatsReplySerializable(Datapath, 
  body = OFPGroupStats(
    group_id = 37135343,
    ref_count = 30334666,
    packet_count = 16467336,
    byte_count = 31159107,
    duration_sec = 18179039,
    duration_nsec = 36282180,
    bucket_stats = [
      OFPBucketCounter(packet_count=3575169166, byte_count=2156878186),
      OFPBucketCounter(packet_count=3664701344, byte_count=998359161)
    ]
  )
)

buckets = [ OFPBucket(actions=one_action) ]
msgs["OfpGroupDescStatsReply"] = OFPGroupDescStatsReplySerializable(Datapath, 
  body = OFPGroupDescStats(
    type_ = OFPGT_SELECT,
    group_id = 321347,
    buckets = buckets
  )
)

msgs["OfpGroupFeaturesStatsReply"] = OFPGroupFeaturesStatsReplySerializable(Datapath, 
  body = OFPGroupFeaturesStats(
    types = (1 << OFPGT_INDIRECT) | (1 << OFPGT_FF), 
    capabilities = OFPGFC_SELECT_WEIGHT | OFPGFC_CHAINING,
    max_groups = [ 100, 0, 200, 0 ],
    actions = [
      (1 << OFPAT_GROUP) | (1 << OFPAT_POP_PBB),
      0, 
      (1 << OFPAT_PUSH_MPLS) | (1 << OFPAT_PUSH_PBB),
      0
    ]
  )
)

msgs["OfpMeterStatsReply"] = OFPMeterStatsReplySerializable(Datapath, 
  body = OFPMeterStats(
    meter_id = 356936,
    flow_count = 381305,
    packet_in_count = 283995,
    byte_in_count = 28555,
    duration_sec = 382212,
    duration_nsec = 139569,
    band_stats = [
      OFPMeterBandStats( 137645, 330608),
      OFPMeterBandStats( 92874353, 1254987)
    ]
  )
)

msgs["OfpMeterConfigStatsReply"] = OFPMeterConfigStatsReplySerializable(Datapath, 
  body = OFPMeterConfigStats(
    flags = OFPMF_KBPS | OFPMF_BURST,
    meter_id = 19857,
    bands = [ ]
  )
)

msgs["OfpMeterFeaturesStatsReply"] = OFPMeterFeaturesStatsReplySerializable(Datapath, 
  body = OFPMeterFeaturesStats(
    max_meter = 987234,
    band_types = (1 << OFPMBT_DROP) | ( 1 << OFPMBT_DSCP_REMARK ),
    capabilities = OFPMF_KBPS | OFPMF_BURST, 
    max_bands = 100,
    max_color = 200
  )
)

tbs = OFPTableFeaturesStats(
  table_id=1, 
  name="Init Table", 
  metadata_match=0, 
  metadata_write=0, 
  config=3, # OFPTC_DEPRECATED_MASK, 
  max_entries=10, 
  properties=one_table_property 
)

msgs["OfpTableFeaturesStatsReply"] = OFPTableFeaturesStatsReplySerializable(Datapath, body = [ tbs ] )

msgs["OfpPortDescStatsReply"] = OFPPortDescStatsReplySerializable(Datapath, body = [ ports ] )

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
# OFPT_METER_MOD

msgs["OfpMeterMod"] = OFPMeterMod(Datapath,
  command = OFPMC_ADD,
  flags = OFPMF_KBPS | OFPMF_BURST,
  meter_id = 19857,
  bands = [
    OFPMeterBandDrop(rate=187236, burst_size=4345234),
    OFPMeterBandDscpRemark(rate=234214, burst_size=2359834, prec_level=66)
  ]
)

####################################################################################
# Finally, dump them all out.

for msg_name, msg in msgs.iteritems():
  msg.serialize()
  hex_file_name = 'data/openflow0x04/{0}.hex'.format(msg_name)
  with open(hex_file_name, 'w') as f:
    f.write( binascii.hexlify(msg.buf) )
