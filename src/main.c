#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>
#include <ctype.h>
#include <string.h>
#include <unistd.h>

#include "gram_util.h"
#include "gram.tab.h"
#include "ast.h"
#include "lex.h"
#include "ext.h"
#include "state.h"
#include "util.h"
#include "verify.h"

/* XXX */
#define INCLUDE_ENVVAR "XR0_INCLUDES"
#define OUTPUT_PATH "0.c"
#define PREPROC_CMD_TEMPLATE "cc %s -nostdinc -E -xc %s"
#define PREPROC_CMD_BASE_LEN (strlen(PREPROC_CMD_TEMPLATE) - 4)

int
yyparse();

enum execmode { EXECMODE_VERIFY, EXECMODE_STRIP, };

struct config {
	char *infile;
	char *outfile;
	struct string_arr *includedirs;
	bool verbose;
	enum execmode mode;

	char *sortfunc;
	bool sort;
};

static struct string_arr *
default_includes();

struct config
parse_config(int argc, char *argv[])
{
	enum execmode mode = EXECMODE_VERIFY;
	bool verbose = false;
	bool sort = false;
	struct string_arr *includedirs = default_includes();
	char *outfile = OUTPUT_PATH;
	char *sortfunc = NULL;
	int opt;
	while ((opt = getopt(argc, argv, "vso:t:I:")) != -1) {
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
		case 's':
			mode = EXECMODE_STRIP;
			break;
		case 't':
			sortfunc = optarg;
			sort = true;
			break;
		default:
			fprintf(stderr, "Usage: %s [-I libx] input_file\n", argv[0]);
			exit(EXIT_FAILURE);
		}
	}
	if (optind >= argc) {
		fprintf(stderr, "must provide input as string\n");
		exit(EXIT_FAILURE);
	}
	return (struct config) {
		.mode		= mode,
		.infile		= argv[optind],
		.outfile	= outfile,
		.includedirs	= includedirs,
		.verbose	= verbose,
		.sort		= sort,
		.sortfunc	= sortfunc,
	};
}


static struct string_arr *
default_includes()
{
	struct string_arr *dirs = string_arr_create();
	char *env = getenv(INCLUDE_ENVVAR);
	if (env) {
		string_arr_append(dirs, env);
	}
	return dirs;
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
verifyproto(struct ast_function *f, int n, struct ast_externdecl **decl);

void
pass0(struct ast *root, struct externals *ext)
{
	/* TODO:
	 * - enforce syntax rules
	 */
	for (int i = 0; i < root->n; i++) {
		struct ast_externdecl *decl = root->decl[i];
		if (!ast_externdecl_isfunction(decl)) {
			ast_externdecl_install(decl, ext);
			continue;
		}
		struct ast_function *f = ast_externdecl_as_function(decl);
		if (ast_function_isaxiom(f)) {
			ast_externdecl_install(decl, ext);
			continue;
		}
		if (ast_function_isproto(f)) {
			if (!verifyproto(f, root->n, root->decl)) {
				exit(EXIT_FAILURE);
			}
			ast_externdecl_install(decl, ext);
			continue;
		}
		struct ast_function *stitched = ast_protostitch(f, ext);
		ast_externdecl_install(
			ast_functiondecl_create(ast_function_copy(stitched)),
			ext
		);
	}
}

void
pass1(struct ast *root, struct externals *ext)
{
	struct error *err;
	for (int i = 0; i < root->n; i++) {
		struct ast_externdecl *decl = root->decl[i];
		if (!ast_externdecl_isfunction(decl)) {
			continue;
		}
		struct ast_function *f = ast_externdecl_as_function(decl);
		if (ast_function_isaxiom(f) || ast_function_isproto(f)) {
			continue;
		}
		/* XXX: ensure that verified functions always have an abstract */
		assert(ast_function_abstract(f));

		if ((err = ast_function_verify(f, ext))) {
			fprintf(stderr, "%s\n", err->msg);
			exit(EXIT_FAILURE);
		}
		v_printf("qed %s\n", ast_function_name(f));
	}
}

static bool
proto_defisvalid(struct ast_function *f1, struct ast_function *f2);

static bool
verifyproto(struct ast_function *proto, int n, struct ast_externdecl **decl)
{
	struct ast_function *def;
	int count = 0;

	char *pname = ast_function_name(proto);
	for (int i = 0; i < n; i++) {
		struct ast_externdecl *decl = root->decl[i];
		if (!ast_externdecl_isfunction(decl)) {
			continue;
		}
		struct ast_function *d = ast_externdecl_as_function(decl);
		/* skip axioms and declarations */
		if (ast_function_isaxiom(d) || ast_function_isproto(d)) {
			continue;
		}	
		if (strcmp(pname, ast_function_name(d)) == 0) {
			def = d;
			count++;
		}
	}
	if (count == 1) {
		if (proto_defisvalid(proto, def)) {
			return true;
		}
		fprintf(
			stderr,
			"function `%s' prototype and definition abstracts mismatch\n", 
			pname
		);
	} else if (count == 0) {
		fprintf(stderr, "function `%s' missing definition\n", pname);
	} else if (count > 1) {
		fprintf(stderr, "function `%s' has multiple definitions\n", pname);
	}
	return false;
}

static bool
proto_defisvalid(struct ast_function *proto, struct ast_function *def)
{
	struct ast_block *proto_abs = ast_function_abstract(proto),
			 *def_abs = ast_function_abstract(def);

	bool abs_match = strcmp(ast_block_str(proto_abs, ""), ast_block_str(def_abs, "")) == 0,
	     protoabs_only = proto_abs && ast_function_absisempty(def); 
	if (abs_match || protoabs_only) {
		return true;
	}
	return false;
}

static int
verify(struct config *c);

static int
strip(struct config *c);

int
main(int argc, char *argv[])
{
	extern int VERBOSE_MODE;

	struct config c = parse_config(argc, argv);
	VERBOSE_MODE = c.verbose;

	switch (c.mode) {
	case EXECMODE_VERIFY:
		return verify(&c);
	case EXECMODE_STRIP:
		return strip(&c);
	default:
		assert(false);
	}
}

static int
verify(struct config *c)
{
	/* preprocess */
	extern FILE *yyin;
	yyin = preprocess(c->infile, c->includedirs);

	/* lex and parse */
	lex_begin();
	yyparse();
	yylex_destroy();
	lex_finish();

	/* TODO: move table from lexer to pass1 */
	struct externals *ext = externals_create();

	/* setup externals */
	pass0(root, ext);

	/* if -s param specified output topological eval order */
	if (c->sort) {
		/* TODO: pass up error conditions */
		if (!c->sortfunc) {
			fprintf(stderr, "supply function to `-t' flag to evaluate dependencies for");
			exit(EXIT_FAILURE);
		}
		struct string_arr *order = ast_topological_order(c->sortfunc, ext);
		/* TODO: our tests run 2>&1 > /dev/null */
		fprintf(stderr, "%s\n", string_arr_str(order));
	} else { 
		/* TODO: verify in topological order */
		pass1(root, ext);
	}

	externals_destroy(ext);
	ast_destroy(root);

	return 0;
}

bool
isvblock(char c, FILE *);

void
skipvblock(FILE *);


static int
strip(struct config *config)
{
	FILE *in = fopen(config->infile, "rb"),
	     *out = fopen(config->outfile, "w");

	char c;

	while ((c = fgetc(in)) != EOF) {
		if (isvblock(c, in)) {
			skipvblock(in);
		} else {
			fputc(c, out);
		}
	}

	fclose(in);
	fclose(out);

	return 0;
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
