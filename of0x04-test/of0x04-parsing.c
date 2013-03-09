#include <stdlib.h>
#include <stdio.h>

#include "ofl-messages.h"

#define BUF_SIZE 8192

size_t read_all(FILE *file, uint8_t **buf)
{
	size_t buf_len;
	*buf = NULL;

	fseek(file, 0, SEEK_END);
	buf_len = ftell(file);
	rewind(file);

	uint8_t *data = (uint8_t*) malloc(buf_len);
	fread(data, buf_len, 1, file);
	if (ferror(file)) {
		fprintf(stderr, "Cannot read msg file.\n");
		return -1;		
	}
	*buf = data;
	return buf_len;
}

int main(int argc, char **argv)
{
	uint8_t *buf;
	size_t buf_len = 0;
	ofl_err err;
	struct ofl_msg_header *msg = NULL;
	uint32_t xid;

	if (argc < 2) {
		fprintf(stderr, "Expecting msg file.\n");
		return 1;
	}

	time_init();
	vlog_init();
	vlog_set_verbosity(NULL);

	FILE *msg_file = fopen(argv[1], "r");
	if (msg_file == NULL) {
		fprintf(stderr, "Cannot open msg file.\n");
		return 1;
	}
	buf_len = read_all(msg_file, &buf);
	printf("buf len = %ld\n", buf_len);

	err = ofl_msg_unpack(buf, buf_len, &msg, &xid, NULL);
	free(buf);
	if (err == 0) {
		printf("Success!\n");
		ofl_msg_print(stdout, msg, NULL);
		printf("\n");
	} else {
		printf("Failed :-( error type: %d code %d\n", ofl_error_type(err), ofl_error_code(err));
	}
	ofl_msg_free(msg, NULL);

	return 0;
}

