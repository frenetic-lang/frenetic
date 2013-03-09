#include <stdlib.h>
#include <stdio.h>

#include "ofl-messages.h"
#include "oxm-match.h"
#include "hash.h"

/*
    let msg2 = FlowMod {
      cookie = ({value = 0xaabbccL; mask = None} : uint64 mask);
      table_id = (0x12 : tableId);
      command = AddFlow;
      idle_timeout = Permanent;
      hard_timeout = Permanent;
      priority = (0x3456 : uint16);
      buffer_id = None;
      out_port = Some (PhysicalPort (0x7890l));
      out_group = None;
      flags = {send_flow_rem = false; check_overlap = false; reset_counts = false; no_pkt_counts = false; no_byt_counts = false};
      ofp_match = [OxmInPort (0x7891l)];
      instructions = [WriteActions ([Output (PhysicalPort (0x7890l))])]
    } in
*/

static
void make_msg2()
{
	uint32_t port = 0x7891;
	struct ofl_match_tlv oxm1;
	oxm1.hmap_node.hash = 0;
	oxm1.hmap_node.next = HMAP_NODE_NULL;
	oxm1.header = OXM_OF_IN_PORT;
	oxm1.value = (uint8_t *) &port;

	struct ofl_match match;
	match.header.type = OFPMT_OXM;
	match.header.length = 8;
	hmap_init(&match.match_fields);
	hmap_insert(&match.match_fields, (struct hmap_node *) &oxm1, hash_int(OXM_OF_IN_PORT, 0));

	struct ofl_action_output act1;
	act1.header.type = OFPAT_OUTPUT;
	act1.header.len = sizeof(act1);
	act1.port = 0x7890;
	act1.max_len = 0;

	struct ofl_action_header *actions[1];
	actions[0] = (struct ofl_action_header *) &act1;

	struct ofl_instruction_actions ins1;
	ins1.header.type = OFPIT_WRITE_ACTIONS;
	ins1.actions_num = 1;
	ins1.actions = actions;

	struct ofl_instruction_header *instructions[1];
	instructions[0] = (struct ofl_instruction_header *) &ins1;

	struct ofl_msg_flow_mod fm;
	fm.header.type = OFPT_FLOW_MOD;
	fm.cookie = 0xaabbcc;
	fm.cookie_mask = 0;
	fm.table_id = 0x12;
	fm.command = OFPFC_ADD;
	fm.idle_timeout = 0;
	fm.hard_timeout = 0;
	fm.priority = 0x3456;
	fm.buffer_id = -1;
	fm.out_port = 0x7890;
	fm.out_group = 0;
	fm.flags = 0;
	fm.match = (struct ofl_match_header *) &match;
	fm.instructions_num = 1;
	fm.instructions = instructions;

	uint8_t *buf;
	size_t buf_len;
	struct ofl_msg_header *msg = (struct ofl_msg_header *) &fm;
	int err = ofl_msg_pack(msg, 0, &buf, &buf_len, NULL);
	if (err == 0) {
		printf("Success!\n");
		ofl_msg_print(stdout, msg, NULL);
		printf("\n");

		FILE *file = fopen("test-msg-1.3-msg2", "w");
		fwrite(buf, buf_len, 1, file);
		fclose(file);
	} else {
		printf("Failed :-(\n");
	}

}

/*
let action1 : action = SetField (OxmVlanVId ({value = 0x1234; mask = None})) in
    let action2 : action = Output (PhysicalPort (0x2345l)) in
    let bucket1 : bucket = {weight = 0; watch_port = Some (0x67l); watch_group = None; actions = [action2]} in
    let msg3 = GroupMod (AddGroup (FF, 0x12l,
      [bucket1])) in
*/

static
void make_msg3()
{
	uint16_t vlan = 0x1234;
	struct ofl_match_tlv oxm1;
	oxm1.hmap_node.hash = 0;
	oxm1.hmap_node.next = HMAP_NODE_NULL;
	oxm1.header = OXM_OF_VLAN_VID;
	oxm1.value = (uint8_t *) &vlan;

	struct ofl_action_set_field act1;
	act1.header.type = OFPAT_SET_FIELD;
	act1.header.len = 6;
	act1.field = &oxm1;

	struct ofl_action_output act2;
	act2.header.type = OFPAT_OUTPUT;
	act2.header.len = sizeof(act2);
	act2.port = 0x2345;
	act2.max_len = 0;

	struct ofl_action_header *actions[2];
	actions[0] = (struct ofl_action_header *) &act1;
	actions[1] = (struct ofl_action_header *) &act2;

	struct ofl_bucket buc1;
	buc1.weight = 0;
	buc1.watch_port = 0x67;
	buc1.watch_group = OFPG_ANY;
	buc1.actions_num = 2;
	buc1.actions = actions;

	struct ofl_bucket *buckets[1];
	buckets[0] = (struct ofl_bucket *) &buc1;

	struct ofl_msg_group_mod gm;
	gm.header.type = OFPT_GROUP_MOD;
	gm.command = OFPGC_ADD;
	gm.type = OFPGT_FF;
	gm.group_id = 0x12;
	gm.buckets_num = 1;
	gm.buckets = buckets;

	uint8_t *buf;
	size_t buf_len;
	struct ofl_msg_header *msg = (struct ofl_msg_header *) &gm;
	int err = ofl_msg_pack(msg, 0, &buf, &buf_len, NULL);
	if (err == 0) {
		printf("Success!\n");
		ofl_msg_print(stdout, msg, NULL);
		printf("\n");

		FILE *file = fopen("test-msg-1.3-msg3", "w");
		fwrite(buf, buf_len, 1, file);
		fclose(file);
	} else {
		printf("Failed :-(\n");
	}

}

int main(int argc, char **argv)
{

	time_init();
	vlog_init();
	vlog_set_verbosity(NULL);

	make_msg2();
	make_msg3();
	return 0;
}
