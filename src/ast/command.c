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

static void
command_destroy(struct command *cmd)
{
	string_arr_destroy(cmd->args);
	free(cmd);
}

bool should_continue = false;

static struct command *
getcmd(char *debugsep);

static struct error *
command_continue_exec(struct verifier *);

static struct error *
command_verify_exec(struct verifier *, struct command *);

static struct ast_expr *
command_arg_toexpr(struct command *);

struct error *
command_next(struct verifier *p, char *debugsep)
{
	struct error *err;

	if (should_continue) {
		should_continue = false;
		return command_continue_exec(p);
	}
	struct command *cmd = getcmd(debugsep);
	switch (cmd->kind) {
	case COMMAND_STEP:
		err = verifier_progress(p, progressor_step());
		break;
	case COMMAND_NEXT:
		err = verifier_progress(p, progressor_next());	
		break;
	case COMMAND_VERIFY:
		err = command_verify_exec(p, cmd);
		break;
	case COMMAND_CONTINUE:
		err = command_continue_exec(p);
		break;	
	case COMMAND_HELP:
	case COMMAND_BREAKPOINT_SET:
	case COMMAND_BREAKPOINT_LIST:
		err = NULL;
		break;
	case COMMAND_QUIT:
		exit(0);
	default:
		assert(false);
	}

	command_destroy(cmd);
	return err;
}

static struct error *
command_continue_exec(struct verifier *p)
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

static struct error *
command_verify_exec(struct verifier *p, struct command *cmd)
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
yyparse();

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
process_command(char *cmd, char *debugsep);

static struct command *
process_commandwithargs(char *cmd, char *args, char *debugsep);

static struct command *
getcmd(char *debugsep)
{
	d_printf("(0db) %s", debugsep);
	char line[MAX_LINELEN];
	char cmd[MAX_COMMANDLEN];
	char args[MAX_ARGSLEN];
	if (!fgets(line, MAX_LINELEN, stdin)) {
		fprintf(stderr, "error: cannot read line\n");
		exit(EXIT_FAILURE);
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
process_command(char *cmd, char *sep)
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
		return getcmd(sep);
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
	d_printf("List of commands:\n");
	d_printf("step -- Step to next logical statement.\n");
	d_printf("next -- Step over.\n");
	d_printf("break -- Set breakpoint.\n");
	d_printf("continue -- Step until breakpoint, error or end.\n");
	d_printf("quit -- Quit.\n\n");
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
command_help(struct string_arr *args, char *debugsep);

static bool
command_isbreak(char *cmd);

static struct command *
command_break(struct string_arr *args, char *debugsep);

static bool
command_isverify(char *cmd);

static struct command *
command_verify(char *arg);

static struct command *
process_commandwithargs(char *cmd, char *args, char *debugsep)
{
	struct string_arr *args_tk = args_tokenise(dynamic_str(args));
	if (args_tk == NULL) {
		fprintf(stderr, "invalid command args: %s\n", args);
		return getcmd(debugsep);
	}
	if (command_ishelp(cmd)) {
		return command_help(args_tk, debugsep);
	} else if (command_isbreak(cmd)) {
		return command_break(args_tk, debugsep);	
	} else if (command_isverify(cmd)) {
		return command_verify(args);
	} else {
		d_printf("unknown command `%s'\n", cmd);
		return getcmd(debugsep);
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
command_help(struct string_arr *args, char *debugsep)
{
	if (string_arr_n(args) != 1) {
		d_printf("`help' expects single argument\n");
		return getcmd(debugsep);
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
		return getcmd(debugsep);
	}
	return command_create(COMMAND_HELP);
}

static void
command_help_step()
{
	d_printf("Step to the next logical statement, entering\n");
	d_printf("the current one when appropriate.\n");
	d_printf("Usage: s(tep)\n\n");
}

static void
command_help_next()
{
	d_printf("Step over the current statement.\n");
	d_printf("Usage: n(ext)\n\n");
}

static void
command_help_continue()
{
	d_printf("Step until breakpoint, error or end.\n");
	d_printf("Usage: c(ontinue)\n\n");
}

static void
command_help_break()
{
	d_printf("break -- Set breakpoint.\n");
	d_printf("Usage: b(reak) [LINE_NUMBER]\n\n");

	d_printf("list -- List all breakpoints.\n");
	d_printf("Usage: b(reak) list\n\n");

	d_printf("Note: Currently breakpoints can only be\n");
	d_printf("set on statements.\n\n");
}

static void
command_help_quit()
{
	d_printf("Quit.\n");
	d_printf("Usage: q(uit)\n");
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
command_break(struct string_arr *args, char *debugsep)
{
	if (string_arr_n(args) != 1) {
		fprintf(stderr, "`break' expects single argument\n");
		return getcmd(debugsep);
	}
	char *arg = string_arr_s(args)[0];
	if (break_argisset(arg)) {
		return break_set(arg);
	} else if (break_argislist(arg)) {
		return break_list();
	} else {
		fprintf(stderr, "`break' received unknown argument\n");
		return getcmd(debugsep);
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

static bool
command_isverify(char *cmd)
{
	return strcmp(cmd, "v") == 0 || strcmp(cmd, "verify") == 0;
}

static struct command *
command_verify(char *arg)
{
	struct string_arr *sarr = string_arr_create();
	string_arr_append(sarr, dynamic_str(arg));
	return command_create_withargs(COMMAND_VERIFY, sarr);
}

static struct command *
break_list()
{
	d_printf("%s\n", breakpoint_list());
	return command_create(COMMAND_BREAKPOINT_LIST);
}
