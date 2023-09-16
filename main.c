#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>
#include <string.h>
#include <unistd.h>
#include "gram_util.h"
#include "gram.tab.h"
#include "ast.h"
#include "lex.h"
#include "state.h"
#include "util.h"
#include "verify.h"

#define OUTPUT_PATH "0.c"

#define PREPROC_CMD_TEMPLATE "cc -I %s -nostdinc -E -xc %s"
#define PREPROC_CMD_BASE_LEN (strlen(PREPROC_CMD_TEMPLATE) - 4)

int yyparse();

struct config {
	char *infile;
	char *outfile;
	char *includedir;
	bool verbose;
};

struct config
parse_config(int argc, char *argv[])
{
	bool verbose = false;
	char *includedir = NULL;
	char *outfile = OUTPUT_PATH;
	int opt;
	while ((opt = getopt(argc, argv, "vo:I:")) != -1) {
		switch (opt) {
		case 'I':
			includedir = optarg;
			break;
		case 'o':
			outfile = optarg;
			break;
		case 'v':
			verbose = true;
			break;
		default:
			fprintf(stderr, "Usage: %s [-o output] input_file\n", argv[0]);
			exit(EXIT_FAILURE);
		}
	}
	if (!includedir) {
		fprintf(stderr, "must provide include directory\n");
		exit(EXIT_FAILURE);
	}
	if (optind >= argc) {
		fprintf(stderr, "must provide input as string\n");
		exit(EXIT_FAILURE);
	}
	return (struct config) {
		.infile		= argv[optind],
		.outfile	= outfile,
		.includedir	= includedir,
		.verbose	= verbose,
	};
}

char *
preprocesscmd_fmt(char *includedir, char *infile)
{
	int len = PREPROC_CMD_BASE_LEN + strlen(includedir) + strlen(infile) + 1;
	char *s = malloc(sizeof(char) * len);
	snprintf(s, len, PREPROC_CMD_TEMPLATE, includedir, infile);
	return s;
}

FILE *
open_preprocessor(char *infile, char *includedir)
{
	char *cmd = preprocesscmd_fmt(includedir, infile);
	FILE *pipe = popen(cmd, "r");
	free(cmd);
	return pipe;
}

FILE *
preprocess(char *infile, char *includedir)
{
	FILE *pipe = open_preprocessor(infile, includedir);
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

struct ast *root;

static bool
should_verify(struct ast_externdecl *decl)
{
	if (decl->kind != EXTERN_FUNCTION) {
		/* TODO: install global var in table */
		return false;
	}
	return !ast_function_isaxiom(decl->u.function);
}

static void
install_declaration(struct ast_externdecl *decl, struct map *extfunc)
{
	/* TODO: install global vars in table */
	if (decl->kind != EXTERN_FUNCTION) {
		return; /* XXX */
	}
	struct ast_function *f = decl->u.function;
	map_set(extfunc, dynamic_str(f->name), ast_function_copy(f));
}


void
pass1(struct ast *root, struct map *extfunc)
{
	struct error *err;
	/* TODO:
	 * - enforce syntax rules
	 * - check that sfuncs have no bodies
	 * - unify declarations and definitions so that each function appears
	 *   once in the array passed to verify
	 * - check that chains do not have contradictory operators
	 */
	for (int i = 0; i < root->n; i++) {
		struct ast_externdecl *decl = root->decl[i];
		install_declaration(decl, extfunc);
		if (!should_verify(decl)) {
			continue;
		}
		struct ast_function *f = decl->u.function;
		/* XXX: ensure that verified functions always have an abstract */
		if (!f->abstract) {
			f->abstract = ast_block_create(NULL, 0, NULL, 0);
		}
		if ((err = function_verify(f, extfunc))) {
			fprintf(stderr, "%s", err->msg);
			exit(EXIT_FAILURE);
		}
	}
}

int
main(int argc, char *argv[])
{
	/* read file and preprocess */
	extern FILE *yyin;
	struct config c = parse_config(argc, argv);
	yyin = preprocess(c.infile, c.includedir);

	/* lex and parse */
	lex_begin();
	verbose = c.verbose;
	yyparse();
	yylex_destroy();
	lex_finish();

	/* TODO: move table from lexer to pass1 */
	struct map *extfunc = map_create();
	pass1(root, extfunc);
	for (int i = 0; i < extfunc->n; i++) {
		ast_function_destroy((struct ast_function *) extfunc->entry[i].value);
	}
	map_destroy(extfunc);

	ast_destroy(root);
}
