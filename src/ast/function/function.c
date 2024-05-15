#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include <ctype.h>

#include "ast.h"
#include "lex.h"
#include "function.h"
#include "stmt/stmt.h"
#include "intern.h"
#include "object.h"
#include "props.h"
#include "path.h"
#include "state.h"
#include "type/type.h"
#include "stmt/stmt.h"
#include "ext.h"
#include "util.h"
#include "breakpoint.h"

struct ast_function {
	bool isaxiom;

	struct ast_type *ret;

	char *name;
	/* parameters */
	int nparam;
	struct ast_variable **param;

	struct ast_block *abstract, 
			 *body;
};

struct ast_function *
ast_function_create(
	bool isaxiom,
	struct ast_type *ret,
	char *name, 
	int nparam,
	struct ast_variable **param,
	struct ast_block *abstract, 
	struct ast_block *body)
{
	struct ast_function *f = malloc(sizeof(struct ast_function));
	f->isaxiom = isaxiom;
	f->ret = ret;
	f->name = name;
	f->nparam = nparam;
	f->param = param;
	assert(abstract);
	f->abstract = abstract;
	f->body = body;
	return f;
}

void
ast_function_destroy(struct ast_function *f)
{
	ast_type_destroy(f->ret);
	for (int i = 0; i < f->nparam; i++) {
		ast_variable_destroy(f->param[i]);
	}
	ast_block_destroy(f->abstract);
	if (f->body) {
		ast_block_destroy(f->body);
	}
	free(f->param);
	free(f->name);
	free(f);
}

char *
ast_function_str(struct ast_function *f)
{
	struct strbuilder *b = strbuilder_create();
	if (f->isaxiom) {
		strbuilder_printf(b, "axiom ");
	}
	char *ret = ast_type_str(f->ret);
	strbuilder_printf(b, "%s\n", ret);
	free(ret);
	strbuilder_printf(b, "%s(", f->name);
	for (int i = 0; i < f->nparam; i++) {
		char *v = ast_variable_str(f->param[i]);
		char *space = (i + 1 < f->nparam) ? ", " : "";
		strbuilder_printf(b, "%s%s", v, space);
		free(v);
	}
	char *abs = ast_block_absstr(f->abstract, 1);
	strbuilder_printf(b, ") ~ %s", abs);
	free(abs);
	if (f->body) {
		char *body = ast_block_str(f->body, 1);
		strbuilder_printf(b, "%s", body);
		free(body);
	} else {
		strbuilder_printf(b, ";");
	}
	strbuilder_printf(b, "\n");
	return strbuilder_build(b);
}

void
ast_function_setname(struct ast_function *f, char *name)
{
	free(f->name);
	f->name = name;
}

char *
ast_function_name(struct ast_function *f)
{
	return f->name;
}

struct ast_function *
ast_function_copy(struct ast_function *f)
{
	assert(f);
	struct ast_variable **param = malloc(sizeof(struct ast_variable *) * f->nparam);
	for (int i = 0; i < f->nparam; i++) {
		param[i] = ast_variable_copy(f->param[i]);
	}
	return ast_function_create(
		f->isaxiom,
		ast_type_copy(f->ret),
		dynamic_str(f->name),
		f->nparam,
		param,
		ast_block_copy(f->abstract),
		f->body ? ast_block_copy(f->body) : NULL
	);
}

bool
ast_function_isaxiom(struct ast_function *f)
{
	return f->isaxiom;
}

bool
ast_function_isproto(struct ast_function *f)
{
	return f->abstract && !f->body;
}

bool
ast_function_isvoid(struct ast_function *f)
{
	return ast_type_isvoid(f->ret);
}

bool
ast_function_absisempty(struct ast_function *f)
{
	return ast_block_ndecls(f->abstract) == 0 && ast_block_nstmts(f->abstract) == 0;
}

struct ast_type *
ast_function_type(struct ast_function *f)
{
	return f->ret;
}

struct ast_block *
ast_function_body(struct ast_function *f)
{
	if (!f->body) {
		fprintf(stderr, "cannot find body for `%s'\n", f->name);
	}
	assert(f->body);
	return f->body;
}

struct ast_block *
ast_function_abstract(struct ast_function *f)
{
	assert(f->abstract);
	return f->abstract;
}

int
ast_function_nparams(struct ast_function *f)
{
	return f->nparam;
}

struct ast_variable **
ast_function_params(struct ast_function *f)
{
	return f->param;
}

struct ast_function *
ast_function_protostitch(struct ast_function *f, struct externals *ext)
{
	struct ast_function *proto = externals_getfunc(ext, f->name);

	if (proto && proto->abstract) {
		f->abstract = ast_block_copy(proto->abstract);
	}
	/* XXX: leaks */
	return f;
}

struct error *
ast_function_verify(struct ast_function *f, struct externals *ext)
{
	struct path *path = path_create(f, ext);
	while (!path_atend(path)) {
		struct error *err = path_step(path);
		if (err) {
			return err;
		}
	}
	path_destroy(path);
	return NULL;
}

static struct error *
next(struct path *);

struct error *
ast_function_debug(struct ast_function *f, struct externals *ext)
{
	struct path *path = path_create(f, ext);
	while (!path_atend(path)) {
		d_printf("%s\n", path_str(path));
		struct error *err = next(path);
		if (err) {
			return err;
		}
	}
	path_destroy(path);
	return NULL;
}

enum command_kind {
	COMMAND_STEP,
	COMMAND_NEXT,
	COMMAND_BREAKPOINT_SET,
	COMMAND_CONTINUE,
	COMMAND_QUIT,
};

struct command {
	enum command_kind kind;
	struct string_arr *args;
};

static struct command *
command_create(enum command_kind kind)
{
	struct command *cmd = malloc(sizeof(struct command));	
	cmd->kind = kind;
	cmd->args = string_arr_create();
	return cmd;
}

static struct command *
command_create_withargs(enum command_kind kind, struct string_arr *args)
{
	struct command *cmd = malloc(sizeof(struct command));	
	cmd->kind = kind;
	cmd->args = args;
	return cmd;
}

static void
command_destroy(struct command *cmd)
{
	string_arr_destroy(cmd->args);
	free(cmd);
}

static struct command *
getcmd();

static struct error *
next(struct path *p)
{
	printf("(0db) ");
	struct command *cmd = getcmd();
	printf("bps:\n%s\n", breakpoint_list());
	switch (cmd->kind) {
	case COMMAND_STEP:
		return path_step(p);
	case COMMAND_NEXT:
		return path_next(p);
	case COMMAND_BREAKPOINT_SET:
		return NULL;
	case COMMAND_CONTINUE:
		return path_continue(p);
	case COMMAND_QUIT:
		exit(0);
	default:
		assert(false);
	}
}

static struct error *
setbreakpoint(struct command *cmd)
{
	char *filename = dynamic_str(string_arr_s(cmd->args)[0]);
	int linenum = atoi(string_arr_s(cmd->args)[1]);
	struct error *err = breakpoint_set(filename, linenum);	
	if (err) {
		assert(false);
	}
	return NULL;
}

#define MAX_COMMANDLEN 100
#define MAX_LINELEN 1000
#define MAX_ARGSLEN 100

static struct command *
process_commandwithargs(char *cmd, char *args);

static struct command *
process_command(char *cmd);

static struct command *
getcmd()
{
	char line[MAX_LINELEN];
	char cmd[MAX_COMMANDLEN];
	char args[MAX_ARGSLEN];
	if (!fgets(line, MAX_LINELEN, stdin)) {
		fprintf(stderr, "error: cannot read\n");
		return getcmd();
	}
	char *space = strchr(line, ' ');
	if (space != NULL) {
		*space = '\0';
		strcpy(cmd, line);
		strcpy(args, space+1);
		args[strcspn(args, "\n")] = '\0';
		return process_commandwithargs(cmd, args);
	} else {
		strcpy(cmd, line);
		cmd[strcspn(cmd, "\n")] = '\0';
		return process_command(cmd);
	}	
}

static bool
command_isbreak(char *cmd);

static struct command *
command_break(struct string_arr *args);

static struct string_arr *
args_tokenise(char *args);

static struct command *
process_commandwithargs(char *cmd, char *args)
{
	struct string_arr *args_tk = args_tokenise(args);
	if (args_tk == NULL) {
		fprintf(stderr, "invalid command args: %s\n", args);
		return getcmd();
	}
	if (command_isbreak(cmd)) {
		return command_break(args_tk);	
	}
}

static bool
command_isbreak(char *cmd)
{
	return strcmp(cmd, "b") == 0 || strcmp(cmd, "break") == 0;
}

static struct string_arr *
break_argsplit(char *arg);

static struct command *
command_break(struct string_arr *args)
{
	if (string_arr_n(args) != 1) {
		fprintf(stderr, "`break' expects single argument\n");
		return getcmd();
	}
	struct string_arr *split = break_argsplit(string_arr_s(args)[0]);
	if (split == NULL) {
		fprintf(stderr, "`break' expects argument format <filename>:<linenum>\n");
		return getcmd();
	}
	char *filename = dynamic_str(string_arr_s(split)[0]);
	int linenum = atoi(string_arr_s(split)[1]);
	struct error *err = breakpoint_set(filename, linenum);
	if (err) {
		fprintf(stderr, "could not set breakpoint: %s", error_str(err));
		return getcmd();
	}
	return command_create(COMMAND_BREAKPOINT_SET);
}

static struct string_arr *
break_argsplit(char *arg)
{
	struct string_arr *split = string_arr_create();

	char fname[MAX_ARGSLEN];
	char linenum[MAX_ARGSLEN];
	char *colon = strchr(arg, ':');
	if (colon == NULL) {
		return NULL;
	} else {
		*colon = '\0';
		strcpy(fname, arg);
		strcpy(linenum, colon+1);
		linenum[strcspn(linenum, "\n")] = '\0';
	}
	string_arr_append(split, dynamic_str(fname));
	string_arr_append(split, dynamic_str(linenum));
	return split;
}

static struct string_arr *
args_tokenise(char *args)
{
	struct string_arr *arg_arr = string_arr_create();
	char *token;
	token = strtok(args, " ");
	while (token != NULL) {
		string_arr_append(arg_arr, dynamic_str(token));	
		token = strtok(NULL, " ");
	}
	return arg_arr;
}

static bool
command_isstep(char *cmd);

static bool
command_isnext(char *cmd);

static bool
command_isquit(char *cmd);

static struct command *
process_command(char *cmd)
{
	if (command_isstep(cmd)) {
		return command_create(COMMAND_STEP);
	} else if (command_isnext(cmd)) {
		return command_create(COMMAND_NEXT);
	} else if (command_isquit(cmd)) {
		return command_create(COMMAND_QUIT);
	} else {
		fprintf(stderr, "unknown command `%s'\n", cmd);
		return getcmd();
	}
}

static bool
command_isstep(char *cmd)
{
	return strcmp(cmd, "s") == 0 || strcmp(cmd, "step") == 0;
}

static bool
command_isnext(char *cmd)
{
	return strcmp(cmd, "n") == 0 || strcmp(cmd, "next") == 0;
}

static bool
command_isquit(char *cmd)
{
	return strcmp(cmd, "q") == 0 || strcmp(cmd, "quit") == 0;
}



static struct error *
inititalise_param(struct ast_variable *v, struct state *);

struct error *
ast_function_initparams(struct ast_function *f, struct state *s)
{
	struct error *err;
	/* declare params and locals in stack frame */	
	int nparams = ast_function_nparams(f);
	struct ast_variable **params = ast_function_params(f);
	for (int i = 0; i < nparams; i++) {
		state_declare(s, params[i], true);
	}

	for (int i = 0; i < nparams; i++) {
		if ((err = inititalise_param(params[i], s))) {
			return err;
		}
	}
	return NULL;
}

struct error *
ast_function_initsetup(struct ast_function *f, struct state *s)
{
	struct preconds_result pre = ast_block_setups(ast_function_abstract(f), s);
	if (pre.err) {
		return pre.err;
	}
	if (!pre.b) {
		return NULL;
	}
	struct frame *setupframe = frame_setup_create(
		"setup",
		pre.b,
		EXEC_SETUP
	);
	state_pushframe(s, setupframe);
	state_initsetup(s, state_frameid(s));
	return NULL;
}

static struct error *
inititalise_param(struct ast_variable *param, struct state *state)
{
	char *name = ast_variable_name(param);
	struct ast_type *t = ast_variable_type(param);

	struct object_res res = state_getobject(state, name);
	assert(!res.err);
	if (object_hasvalue(res.obj)) {
		/* must on the clump or heap */
		//struct value *val = object_as_value(obj);	
		//struct location *loc = value_as_location(val);
		//assert(
		//	location_type(loc) == LOCATION_DEREFERENCABLE ||
		//	location_type(loc) == LOCATION_DYNAMIC
		//);
	} else {
		/* variables that aren't talked about by the preconditions */
		struct value *val = state_vconst(state, t, dynamic_str(name), true);
		object_assign(res.obj, val);
	}
	return NULL;
}

static void
recurse_buildgraph(struct map *g, struct map *dedup, char *fname, struct externals *ext);

struct map *
ast_function_buildgraph(char *fname, struct externals *ext)
{
	struct map *dedup = map_create(),
		   *g = map_create();

	recurse_buildgraph(g, dedup, fname, ext);

	return g;
}

static void
recurse_buildgraph(struct map *g, struct map *dedup, char *fname, struct externals *ext)
{
	struct map *local_dedup = map_create();

	if (map_get(dedup, fname) != NULL) {
		return;
	}
	map_set(dedup, fname, (void *) true);
	struct ast_function *f = externals_getfunc(ext, fname);
	if (!f) {
		/* TODO: pass up an error */
		fprintf(stderr, "function `%s' is not declared\n", fname);	
		exit(EXIT_FAILURE);
	}
	assert(f);

	if (f->isaxiom) {
		return;
	} 

	/* XXX: look in abstracts */
	/* XXX: handle prototypes */
	assert(f->body);
	struct ast_block *body = f->body;
	int nstmts = ast_block_nstmts(body);
	struct ast_stmt **stmt = ast_block_stmts(body);

	assert(stmt);
	struct string_arr *val = string_arr_create();
	for (int i = 0; i < nstmts; i++) {
		struct string_arr *farr = ast_stmt_getfuncs(stmt[i]);		
		if (!farr) {
			continue;
		}

		char **func = string_arr_s(farr); 
		for (int j = 0; j < string_arr_n(farr); j++) {
			/* avoid duplicates */
			if (map_get(local_dedup, func[j]) != NULL) {
				continue;
			}
			
			struct ast_function *f = externals_getfunc(ext, func[j]);
			if (!f->isaxiom) {
				string_arr_append(val, func[j]);	
			}
			map_set(local_dedup, func[j], (void *) true);

			/* recursively build for other funcs */
			recurse_buildgraph(g, dedup, func[j], ext);
		}
	}

	map_set(g, dynamic_str(fname), val);
}

#include "arr.c"
