#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>
#include <string.h>
#include <unistd.h>
#include "gram_util.h"
#include "build/gram.tab.h"
#include "ast.h"
#include "lex.h"
#include "ext.h"
#include "state.h"
#include "util.h"
#include "verify.h"

#define OUTPUT_PATH "0.c"

#define PREPROC_CMD_TEMPLATE "cc %s -nostdinc -E -xc %s"
#define PREPROC_CMD_BASE_LEN (strlen(PREPROC_CMD_TEMPLATE) - 4)

int yyparse();

struct string_arr;

struct string_arr *
string_arr_create();

void
string_arr_destroy(struct string_arr *);

void
string_arr_append(struct string_arr *, char *);

char **
string_arr_s(struct string_arr *);

int
string_arr_n(struct string_arr *);

struct config {
	char *infile;
	char *outfile;
	struct string_arr *includedirs;
	bool verbose;
};

struct config
parse_config(int argc, char *argv[])
{
	bool verbose = false;
	struct string_arr *includedirs = string_arr_create();
	char *outfile = OUTPUT_PATH;
	int opt;
	while ((opt = getopt(argc, argv, "vo:I:")) != -1) {
		switch (opt) {
		case 'I':
			string_arr_append(includedirs, dynamic_str(optarg));
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
	if (optind >= argc) {
		fprintf(stderr, "must provide input as string\n");
		exit(EXIT_FAILURE);
	}
	return (struct config) {
		.infile		= argv[optind],
		.outfile	= outfile,
		.includedirs	= includedirs,
		.verbose	= verbose,
	};
}

struct string_arr {
	int n;
	char **s;
};

struct string_arr *
string_arr_create()
{
	return calloc(1, sizeof(struct string_arr));
}

void
string_arr_destroy(struct string_arr *arr)
{
	for (int i = 0; i < arr->n; i++) {
		free(arr->s[i]);
	}
	free(arr);
}

void
string_arr_append(struct string_arr *arr, char *s)
{
	arr->s = realloc(arr->s, sizeof(char *) * ++arr->n);
	arr->s[arr->n-1] = s;
}

char **
string_arr_s(struct string_arr *arr)
{
	return arr->s;
}

int
string_arr_n(struct string_arr *arr)
{
	return arr->n;
}


char *
genincludes(struct string_arr *includedirs)
{
	struct strbuilder *b = strbuilder_create();
	char **s = string_arr_s(includedirs);
	int n = string_arr_n(includedirs);
	for (int i = 0; i < n; i++) {
		strbuilder_printf(b, " -I %s", s[i]);
	}
	return strbuilder_build(b);
}

char *
preprocesscmd_fmt(struct string_arr *includedirs, char *infile)
{
	char *includes = genincludes(includedirs);
	int len = PREPROC_CMD_BASE_LEN + strlen(includes) + strlen(infile) + 1;
	char *s = malloc(sizeof(char) * len);
	snprintf(s, len, PREPROC_CMD_TEMPLATE, includes, infile);
	return s;
}

FILE *
open_preprocessor(char *infile, struct string_arr *includedirs)
{
	char *cmd = preprocesscmd_fmt(includedirs, infile);
	FILE *pipe = popen(cmd, "r");
	free(cmd);
	return pipe;
}

FILE *
preprocess(char *infile, struct string_arr *includedirs)
{
	FILE *pipe = open_preprocessor(infile, includedirs);
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
install_declaration(struct ast_externdecl *decl, struct externals *ext)
{
	struct ast_function *f;
	struct ast_variable *v;
	struct ast_type *t;

	switch (decl->kind) {
	case EXTERN_FUNCTION:
		f = decl->u.function;
		externals_declarefunc(ext, f->name, f);
		break;
	case EXTERN_VARIABLE:
		printf("variable: %s\n", decl->u.variable->name);
		v = decl->u.variable;
		externals_declarevar(ext, v->name, v);
		break;
	case EXTERN_TYPE:
		t = decl->u.type;
		assert(t->base == TYPE_STRUCT && t->u.structunion.tag);
		externals_declaretype(ext, t->u.structunion.tag, t);
		break;
	default:
		assert(false);
	}
}


void
pass1(struct ast *root, struct externals *ext)
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
		install_declaration(decl, ext);
		if (!should_verify(decl)) {
			continue;
		}
		struct ast_function *f = decl->u.function;
		/* XXX: ensure that verified functions always have an abstract */
		if (!f->abstract) {
			f->abstract = ast_block_create(NULL, 0, NULL, 0);
		}
		if ((err = function_verify(f, ext))) {
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
	yyin = preprocess(c.infile, c.includedirs);

	/* lex and parse */
	lex_begin();
	verbose = c.verbose;
	yyparse();
	yylex_destroy();
	lex_finish();

	/* TODO: move table from lexer to pass1 */
	struct externals *ext = externals_create();
	pass1(root, ext);
	externals_destroy(ext);

	ast_destroy(root);
}
