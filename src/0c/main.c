#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>
#include <string.h>
#include <ctype.h>

/* XXX */
#define PREPROC_CMD_TEMPLATE "c89 -xc %s"
#define PREPROC_CMD_BASE_LEN (strlen(PREPROC_CMD_TEMPLATE) - 2)

FILE *
preprocess(char *infile);

bool
isvblock(char c, FILE *);

void
skipvblock(FILE *);

int
main(int argc, char *argv[])
{
	FILE *tmp = tmpfile();
	FILE *f = fopen(argv[1], "rb");

	char c;

	while ((c = fgetc(f)) != EOF) {
		if (isvblock(c, f)) {
			skipvblock(f);
		} else {
			putchar(c);
		}
	}

	fclose(f);
}

void
skipws(FILE *f);

bool
isvblock(char c, FILE *f)
{
	if (c != '~') {
		return false;
	}

	long pos = ftell(f);

	/* skip whitespace */
	for (c = fgetc(f); isspace(c); c = fgetc(f))
		;

	switch (c) {
	case '[':
		return true;
	case EOF:
		/* EOF will be processed above */
		fseek(f, -1, SEEK_CUR);
		return false;
	default:
		/* found nothing so reset */
		fseek(f, pos, SEEK_SET);
		return false;
	}
}

void
skipvblock(FILE *f)
{
	char c;

	int count = 0; /* counts additional pairs */
	while ((c = fgetc(f)) != ']' || count) {
		switch (c) {
		case '[':
			count++;
			break;
		case ']':
			count--;
			break;
		}
	}
}

FILE *
open_preprocessor(char *infile);

FILE *
preprocess(char *infile)
{
	FILE *pipe = open_preprocessor(infile);
	if (!pipe) {
		fprintf(stderr, "command error\n");
		exit(EXIT_FAILURE);
	}
	FILE *tmp = tmpfile();
	if (!tmp) {
		fprintf(stderr, "cannot create temp file\n");
		exit(EXIT_FAILURE);
	}
	char buf[1024];
	while (fgets(buf, sizeof(buf), pipe) != NULL) {
		fputs(buf, tmp);
	}
	pclose(pipe);
	rewind(tmp);
	return tmp;
}


char *
preprocesscmd_fmt(char *infile);

FILE *
open_preprocessor(char *infile)
{
	char *cmd = preprocesscmd_fmt(infile);
	FILE *pipe = popen(cmd, "r");
	free(cmd);
	return pipe;
}

char *
preprocesscmd_fmt(char *infile)
{
	int len = PREPROC_CMD_BASE_LEN + strlen(infile) + 1;
	char *s = malloc(sizeof(char) * len);
	snprintf(s, len, PREPROC_CMD_TEMPLATE, infile);
	return s;
}
