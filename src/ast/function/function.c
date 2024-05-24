#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include <ctype.h>

#include "gram_util.h"
#include "gram.tab.h"
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
next_command(struct path *);

struct error *
ast_function_debug(struct ast_function *f, struct externals *ext)
{
	struct path *path = path_create(f, ext);
	while (!path_atend(path)) {
		d_printf("%s\n", path_str(path));
		struct error *err = next_command(path);
		if (err) {
			return err;
		}
	}
	path_destroy(path);
	return NULL;
}

enum command_kind {
	COMMAND_HELP,
	COMMAND_STEP,
	COMMAND_NEXT,
	COMMAND_CONTINUE,
	COMMAND_VERIFY,
	COMMAND_QUIT,
	COMMAND_BREAKPOINT_SET,
	COMMAND_BREAKPOINT_LIST,
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


bool should_continue = false;

static struct command *
getcmd();

static struct error *
command_help_exec();

static struct error *
command_continue_exec(struct path *);

static struct error *
command_verify_exec(struct path *, struct command *);

static struct ast_expr *
command_arg_toexpr(struct command *);

static struct error *
next_command(struct path *p)
{
	if (should_continue) {
		should_continue = false;
		return command_continue_exec(p);
	}
	struct command *cmd = getcmd();
	switch (cmd->kind) {
	case COMMAND_STEP:
		return path_step(p);
	case COMMAND_NEXT:
		return path_next(p);	
	case COMMAND_VERIFY:
		return command_verify_exec(p, cmd);
	case COMMAND_CONTINUE:
		return command_continue_exec(p);
	case COMMAND_QUIT:
		exit(0);
	case COMMAND_HELP:
	case COMMAND_BREAKPOINT_SET:
	case COMMAND_BREAKPOINT_LIST:
		return NULL;
	default:
		assert(false);
	}
}

static struct error *
command_help_exec()
{
	return NULL;
}

static struct error *
command_continue_exec(struct path *p)
{
	while (!path_atend(p)) {
		struct error *err = path_step(p);
		if (err) {
			return err;
		}
		struct lexememarker *loc = path_lexememarker(p);
		if (loc && breakpoint_shouldbreak(loc)) {
			return NULL;
		}
	}
	should_continue = true;
	return NULL;
}

static struct error *
command_verify_exec(struct path *p, struct command *cmd)
{
	struct error *err = path_verify(p, command_arg_toexpr(cmd));
	if (err) {
		d_printf("false: %s\n", error_str(err));
	} else {
		d_printf("true\n");
	}
	return NULL;
}

struct ast_expr *YACC_PARSED_EXPR;

static struct ast_expr *
command_arg_toexpr(struct command *c)
{
	extern FILE *yyin;
	extern int LEX_START_TOKEN;

	char *str = string_arr_s(c->args)[0];
	yyin = fmemopen(str, strlen(str), "r"); // Open the buffer for read/write
	if (!yyin) {
		fprintf(stderr, "error opening memory file\n");
		exit(EXIT_FAILURE);
	}

	/* lex and parse */
	LEX_START_TOKEN = START_EXPR;
	lex_begin();
	yyparse();
	yylex_destroy();
	/* lex_finish(); */

	return ast_expr_copy(YACC_PARSED_EXPR);
}


/* getcmd() */

#define MAX_COMMANDLEN 100
#define MAX_LINELEN 1000
#define MAX_ARGSLEN 100

static struct command *
process_command(char *cmd);

static struct command *
process_commandwithargs(char *cmd, char *args);

static struct command *
getcmd()
{
	printf("(0db) ");
	char line[MAX_LINELEN];
	char cmd[MAX_COMMANDLEN];
	char args[MAX_ARGSLEN];
	if (!fgets(line, MAX_LINELEN, stdin)) {
		fprintf(stderr, "error: cannot read\n");
		return getcmd();
	}
	char *space = strchr(line, ' ');
	if (space == NULL) {
		/* ⊢ command no args */
		strcpy(cmd, line);
		cmd[strcspn(cmd, "\n")] = '\0';
		return process_command(cmd);
	} else {
		/* ⊢ command with args */
		*space = '\0';
		strcpy(cmd, line);
		strcpy(args, space+1);
		args[strcspn(args, "\n")] = '\0';
		return process_commandwithargs(cmd, args);	
	}
}

static struct command *
command_create_help();

static bool
command_ishelp(char *cmd);

static bool
command_isstep(char *cmd);

static bool
command_isnext(char *cmd);

static bool
command_iscontinue(char *cmd);

static bool
command_isquit(char *cmd);

static struct command *
process_command(char *cmd)
{
	if (command_ishelp(cmd)) {
		return command_create_help(COMMAND_HELP);
	} else if (command_isstep(cmd)) {
		return command_create(COMMAND_STEP);
	} else if (command_isnext(cmd)) {
		return command_create(COMMAND_NEXT);
	} else if (command_iscontinue(cmd)){
		return command_create(COMMAND_CONTINUE);
	} else if (command_isquit(cmd)) {
		return command_create(COMMAND_QUIT);
	} else {
		d_printf("unknown command `%s'\n", cmd);
		return getcmd();
	}
}

static bool
command_ishelp(char *cmd)
{
	return strcmp(cmd, "h") == 0 || strcmp(cmd, "help") == 0;
}

static struct command *
command_create_help()
{
	d_printf("List of possible commands:\n");
	d_printf("step -- Step through the program operation by operation\n");
	d_printf("next -- Step through the program line by line skipping over nested operations\n");
	d_printf("break -- Set breakpoint to stop on\n");
	d_printf("continue -- Step until reaching a breakpoint, error, undecidable condition or end of the program\n");
	d_printf("quit -- End the debugging session\n\n");
	return command_create(COMMAND_HELP);
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
command_iscontinue(char *cmd)
{
	return strcmp(cmd, "c") == 0 || strcmp(cmd, "continue") == 0;
}

static bool
command_isquit(char *cmd)
{
	return strcmp(cmd, "q") == 0 || strcmp(cmd, "quit") == 0;
}

static struct string_arr *
args_tokenise(char *args);

static struct command *
command_help(struct string_arr *args);

static bool
command_isbreak(char *cmd);

static struct command *
command_break(struct string_arr *args);

static bool
command_isverify(char *cmd);

static struct command *
command_verify(struct string_arr *args);

static struct command *
process_commandwithargs(char *cmd, char *args)
{
	struct string_arr *args_tk = args_tokenise(args);
	if (args_tk == NULL) {
		fprintf(stderr, "invalid command args: %s\n", args);
		return getcmd();
	}
	if (command_ishelp(cmd)) {
		return command_help(args_tk);
	} else if (command_isbreak(cmd)) {
		return command_break(args_tk);	
	} else if (command_isverify(cmd)) {
		return command_verify(args_tk);
	} else {
		d_printf("unknown command `%s'\n", cmd);
		return getcmd();
	}
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

static void
command_help_step();

static void
command_help_next();

static void
command_help_continue();

static void
command_help_break();

static void
command_help_quit();

static struct command *
command_help(struct string_arr *args)
{
	if (string_arr_n(args) != 1) {
		d_printf("`help' expects single argument\n");
		return getcmd();
	}
	char *arg = string_arr_s(args)[0];
	if (command_isstep(arg)) {
		command_help_step();
	} else if (command_isnext(arg)) {
		command_help_next();
	} else if (command_iscontinue(arg)) {
		command_help_continue();
	} else if (command_isbreak(arg)) {
		command_help_break();
	} else if (command_isquit(arg)) {
		command_help_quit();
	} else {
		d_printf("`help' received unknown argument\n");
		return getcmd();
	}
	return command_create(COMMAND_HELP);
}

static void
command_help_step()
{
	d_printf("Step through program, unlike the conventional stepping in a debugger\n");
	d_printf("like gdb, this is a step with respect to Xr0's internal evaluation of\n");
	d_printf("the program rather than over a line in the program text\n");
	d_printf("Usage: step, s\n");
}

static void
command_help_next()
{
	d_printf("Step through program with respect to the lines in the program text, this\n");
	d_printf("might execute several steps internally to end up at the state for the next\n");
	d_printf("line in the program text\n");
	d_printf("Usage: next, n\n");
}

static void
command_help_continue()
{
	d_printf("Steps through program until a breakpoint, an error, an undecidable condition\n");
	d_printf("or the program end is reached\n");
	d_printf("Usage: continue, c\n");
}

static void
command_help_break()
{
	d_printf("Set breakpoints to stop on when running continue.\n");
	d_printf("Usage: break [LINE_NUMBER], b [LINE_NUMBER]\n");
	d_printf("Note: A current limitation is that breakpoints can only be set on statements.\n");
}

static void
command_help_quit()
{
	d_printf("Quit the debugging session.\n");
	d_printf("Usage: quit, q\n");
}

static bool
command_isbreak(char *cmd)
{
	return strcmp(cmd, "b") == 0 || strcmp(cmd, "break") == 0;
}

static bool
break_argisset(char *arg);

static bool
break_argislist(char *arg);

static struct command *
break_set(char *arg);

static struct command *
break_list();

static struct command *
command_break(struct string_arr *args)
{
	if (string_arr_n(args) != 1) {
		fprintf(stderr, "`break' expects single argument\n");
		return getcmd();
	}
	char *arg = string_arr_s(args)[0];
	if (break_argisset(arg)) {
		return break_set(arg);
	} else if (break_argislist(arg)) {
		return break_list();
	} else {
		fprintf(stderr, "`break' received unknown argument\n");
		return getcmd();
	}
}

static bool
isint(const char *str) {
	if (str == NULL || *str == '\0') {
		return false;
	}
	while (*str) {
		if (!isdigit(*str)) {
			return false;
		}
		str++;
	}
	return true;
}

static bool
break_argisset(char *arg)
{
	return isint(arg);
}

static bool
break_argislist(char *arg)
{
	return strcmp(arg, "list") == 0;
}

static struct string_arr *
break_argsplit(char *arg);

static struct command *
break_set(char *arg)
{
	int linenum = atoi(arg);
	struct error *err = breakpoint_set("", linenum);
	if (err) {
		d_printf("could not set breakpoint: %s", error_str(err));
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

static bool
command_isverify(char *cmd)
{
	return strcmp(cmd, "v") == 0 || strcmp(cmd, "verify") == 0;
}

static struct command *
command_verify(struct string_arr *args)
{
	assert(string_arr_n(args) == 1);
	return command_create_withargs(COMMAND_VERIFY, args);
}

static struct command *
break_list()
{
	d_printf("%s\n", breakpoint_list());
	return command_create(COMMAND_BREAKPOINT_LIST);
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
