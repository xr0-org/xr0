#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <assert.h>
#include <ctype.h>

#include "gram_util.h"
#include "gram.tab.h"
#include "lex.h"
#include "ast.h"
#include "breakpoint.h"
#include "command.h"
#include "util.h"
#include "verifier.h"

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

struct command *
command_copy(struct command *old)
{
	struct command *new = command_create(old->kind);
	for (int i = 0; i < string_arr_n(old->args); i++) {
		string_arr_append(
			new->args,
			dynamic_str(string_arr_s(old->args)[i])
		);
	}
	return new;
}

void
command_destroy(struct command *cmd)
{
	string_arr_destroy(cmd->args);
	free(cmd);
}

char *
command_str(struct command *c)
{
	switch (c->kind) {
	case COMMAND_STEP:
		return dynamic_str("step");
	case COMMAND_NEXT:
		return dynamic_str("next");
	default:
		assert(0);
	}
}

bool should_continue = false;

/* getcmd() */

#define MAX_COMMANDLEN 100
#define MAX_LINELEN 1000
#define MAX_ARGSLEN 100

DEFINE_RESULT_TYPE(struct command *, cmd, command_destroy, command_res, false)

static struct command_res *
process_command(char *cmd, char *debugsep);

static struct command_res *
process_commandwithargs(char *cmd, char *args, char *debugsep);

static struct command_res *
getcmd(char *debugsep)
{
	d_printf("(0db) %s", debugsep);
	char line[MAX_LINELEN];
	char cmd[MAX_COMMANDLEN];
	char args[MAX_ARGSLEN];
	if (!fgets(line, MAX_LINELEN, stdin)) {
		if (feof(stdin)) {
			d_printf("EOF encountered, exiting debugger ...");
			exit(0);
		}
		return command_res_error_create(error_printf("error reading line"));
	}
	char *space = strchr(line, ' ');
	if (space == NULL) {
		/* ⊢ command no args */
		strcpy(cmd, line);
		cmd[strcspn(cmd, "\n")] = '\0';
		return process_command(cmd, debugsep);
	} else {
		/* ⊢ command with args */
		*space = '\0';
		strcpy(cmd, line);
		strcpy(args, space+1);
		args[strcspn(args, "\n")] = '\0';
		return process_commandwithargs(cmd, args, debugsep);
	}
}

static bool
command_ishelp(char *cmd);

static bool
command_isstep(char *cmd);

static bool
command_isnext(char *cmd);

static bool
command_iscontinue(char *cmd);

static bool
command_isverify(char *cmd);

static bool
command_isquit(char *cmd);

static bool
command_isbreak(char *cmd);

static struct command_res *
process_command(char *cmd, char *sep)
{
	struct command *c = NULL;
	if (command_ishelp(cmd)) {
		c = command_create(COMMAND_HELP);
	} else if (command_isstep(cmd)) {
		c = command_create(COMMAND_STEP);
	} else if (command_isnext(cmd)) {
		c = command_create(COMMAND_NEXT);
	} else if (command_iscontinue(cmd)){
		c = command_create(COMMAND_CONTINUE);
	} else if (command_isquit(cmd)) {
		c = command_create(COMMAND_QUIT);
	} else {
		return command_res_error_create(
			error_printf(
				"%w unknown command `%s'",
				error_cmdvalidation(),
				cmd
			)
		);
	}
	return command_res_cmd_create(c);
}

static bool
command_ishelp(char *cmd)
{
	return strcmp(cmd, "h") == 0 || strcmp(cmd, "help") == 0;
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
command_isverify(char *cmd)
{
	return strcmp(cmd, "v") == 0 || strcmp(cmd, "verify") == 0;
}

static bool
command_isquit(char *cmd)
{
	return strcmp(cmd, "q") == 0 || strcmp(cmd, "quit") == 0;
}

static bool
command_isbreak(char *cmd)
{
	return strcmp(cmd, "b") == 0 || strcmp(cmd, "break") == 0;
}


/* command_create_withargs */

static struct string_arr *
args_tokenise(char *args);

static struct command_res *
command_help_create(struct string_arr *args, char *debugsep);

static struct command_res *
command_break_create(struct string_arr *args, char *debugsep);

static struct command_res *
command_verify_create(char *arg);

static struct command_res *
process_commandwithargs(char *cmd, char *args, char *debugsep)
{
	struct string_arr *args_tk = args_tokenise(dynamic_str(args));
	if (args_tk == NULL) {
		return command_res_error_create(
			error_printf(
				"%w invalid command args: %s",
				error_cmdvalidation(),
				args
			)
		);
	}
	struct command *c = NULL;
	if (command_ishelp(cmd)) {
		return command_help_create(args_tk, debugsep);
	} else if (command_isbreak(cmd)) {
		return command_break_create(args_tk, debugsep);
	} else if (command_isverify(cmd)) {
		return command_verify_create(args);
	} else {
		return command_res_error_create(
			error_printf(
				"%w unknown command `%s'",
				error_cmdvalidation(),
				cmd
			)
		);
	}
	return command_res_cmd_create(c);
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

static struct command_res *
command_help_create(struct string_arr *args, char *debugsep)
{
	assert(args);
	if (string_arr_n(args) != 1) {
		return command_res_error_create(
			error_printf(
				"%w `help' expects single argument",
				error_cmdvalidation()
			)
		);
	}
	return command_res_cmd_create(command_create_withargs(COMMAND_HELP, args));
}

static bool
break_argisset(char *arg);

static bool
break_argislist(char *arg);

static struct command_res *
command_break_create(struct string_arr *args, char *debugsep)
{
	if (string_arr_n(args) != 1) {
		return command_res_error_create(
			error_printf(
				"%w `break' expects single argument",
				error_cmdvalidation()
			)
		);
	}
	char *arg = string_arr_s(args)[0];
	if (break_argisset(arg)) {
		return command_res_cmd_create(
			command_create_withargs(COMMAND_BREAKPOINT_SET, args)
		);
	} else if (break_argislist(arg)) {
		return command_res_cmd_create(
			command_create(COMMAND_BREAKPOINT_LIST)
		);
	} else {
		return command_res_error_create(
			error_printf(
				"%w `break' received unexpected argument `%s'",
				error_cmdvalidation(),
				arg
			)
		);
	}
}

static bool
isint(const char *);

static bool
break_argisset(char *arg)
{
	return isint(arg);
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
break_argislist(char *arg)
{
	return strcmp(arg, "list") == 0;
}

static struct command_res *
command_verify_create(char *arg)
{
	struct string_arr *sarr = string_arr_create();
	string_arr_append(sarr, dynamic_str(arg));
	return command_res_cmd_create(
		command_create_withargs(COMMAND_VERIFY, sarr)
	);
}


/* command_exec */

static struct command *
command_read(char *debugsep);

static struct error *
help_exec(struct command *);

static struct error *
continue_exec(struct verifier *);

static struct error *
verify_exec(struct verifier *, struct command *);

static struct error *
break_set_exec(struct command *);

struct error *
command_exec(struct verifier *v, char *debugsep)
{
	struct error *err;

	if (should_continue) {
		should_continue = false;
		return continue_exec(v);
	}
	struct command *cmd = command_read(debugsep);
	switch (cmd->kind) {
	case COMMAND_HELP:
		err = help_exec(cmd);
		break;
	case COMMAND_STEP:
		err = verifier_progress(v, progressor_step());
		break;
	case COMMAND_NEXT:
		err = verifier_progress(v, progressor_next());
		break;
	case COMMAND_CONTINUE:
		err = continue_exec(v);
		break;
	case COMMAND_VERIFY:
		err = verify_exec(v, cmd);
		break;
	case COMMAND_QUIT:
		exit(0);
	case COMMAND_BREAKPOINT_SET:
		err = break_set_exec(cmd);
	case COMMAND_BREAKPOINT_LIST:
		err = NULL;
		break;
	default:
		assert(false);
	}

	command_destroy(cmd);
	return err;
}

static struct command *
command_read(char *debugsep)
{
	struct command_res *res = getcmd(debugsep);
	if (command_res_iserror(res)) {
		struct error *err = command_res_as_error(res);
		struct error *cmd_err = error_to_cmdvalidation(command_res_as_error(res));
		if (cmd_err) {
			printf("cmd error: %s", error_str(cmd_err));
			return command_read(debugsep);
		}
		printf("error: %s\n", error_str(err));
		assert(0);
	}
	return command_res_as_cmd(res);
}

static void
help_base(void);

static void
help_step(void);

static void
help_next(void);

static void
help_continue(void);

static void
help_break(void);

static void
help_quit(void);

static struct error *
help_exec(struct command *cmd)
{
	int nargs = string_arr_n(cmd->args);
	if (nargs == 0) {
		help_base();
		return NULL;
	}
	assert(nargs == 1);
	char *arg = string_arr_s(cmd->args)[0];
	if (command_isstep(arg)) {
		help_step();
	} else if (command_isnext(arg)) {
		help_next();
	} else if (command_iscontinue(arg)) {
		help_continue();
	} else if (command_isbreak(arg)) {
		help_break();
	} else if (command_isquit(arg)) {
		help_quit();
	} else {
		d_printf("`help' received unknown argument\n");
	}
	return NULL;
}

static void
help_base()
{
	d_printf("List of commands:\n");
	d_printf("step -- Step to next logical statement.\n");
	d_printf("next -- Step over.\n");
	d_printf("break -- Set breakpoint.\n");
	d_printf("continue -- Step until breakpoint, error or end.\n");
	d_printf("quit -- Quit.\n\n");
}

static void
help_step(void)
{
	d_printf("Step to the next logical statement, entering\n");
	d_printf("the current one when appropriate.\n");
	d_printf("Usage: s(tep)\n\n");
}

static void
help_next(void)
{
	d_printf("Step over the current statement.\n");
	d_printf("Usage: n(ext)\n\n");
}

static void
help_continue(void)
{
	d_printf("Step until breakpoint, error or end.\n");
	d_printf("Usage: c(ontinue)\n\n");
}

static void
help_break(void)
{
	d_printf("break -- Set breakpoint.\n");
	d_printf("Usage: b(reak) [LINE_NUMBER]\n\n");

	d_printf("list -- List all breakpoints.\n");
	d_printf("Usage: b(reak) list\n\n");

	d_printf("Note: Currently breakpoints can only be\n");
	d_printf("set on statements.\n\n");
}

static void
help_quit(void)
{
	d_printf("Quit.\n");
	d_printf("Usage: q(uit)\n\n");
}

static struct error *
continue_exec(struct verifier *p)
{
	while (!verifier_atend(p)) {
		struct error *err = verifier_progress(p, progressor_step());
		if (err) {
			return err;
		}
		struct lexememarker *loc = verifier_lexememarker(p);
		if (loc && breakpoint_shouldbreak(loc)) {
			return NULL;
		}
	}
	should_continue = true;
	return NULL;
}

static struct ast_expr *
command_arg_toexpr(struct command *);

static struct error *
verify_exec(struct verifier *p, struct command *cmd)
{
	struct error *err = verifier_verify(p, command_arg_toexpr(cmd));
	if (err) {
		d_printf("false: %s\n", error_str(err));
	} else {
		d_printf("true\n");
	}
	return NULL;
}

struct ast_expr *YACC_PARSED_EXPR;

int
yyparse(void);

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

static struct error *
break_set_exec(struct command *c)
{
	assert(string_arr_n(c->args) == 1);
	char *arg = string_arr_s(c->args)[0];
	assert(isint(arg));
	int linenum = atoi(arg);
	struct error *err = breakpoint_set("", linenum);
	if (err) {
		return error_printf("could not set breakpoint: %w", err);
	}
	return NULL;
}
