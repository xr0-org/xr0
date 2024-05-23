#ifndef XR0_LEX_H
#define XR0_LEX_H

#define XR0_INCLUDES_SEGMENT "libx/"

struct lexememarker {
	int linenum, column;
	char *filename;
	enum linemarker_flag {
		LM_FLAG_NEW_FILE	= 1 << 0,
		LM_FLAG_RESUME_FILE	= 1 << 1,
		LM_FLAG_SYS_HEADER	= 1 << 2,
		LM_FLAG_IMPLICIT_EXTERN	= 1 << 3,
	} flags;
};

struct lexememarker *
lexloc();

int
lexememarker_linenum(struct lexememarker *);

char *
lexememarker_filename(struct lexememarker *);

struct lexememarker *
lexememarker_copy(struct lexememarker *);

void
lexememarker_destroy(struct lexememarker *);

char *
lexememarker_str(struct lexememarker *);

extern struct ast_stmt *YACC_PARSED_STMT;
extern struct ast *root;
extern struct map *table;

enum ictype {
	IC_NONE = 1 << 0,
	IC_TYPE = 1 << 1,
	IC_PRED = 2 << 2,
};

extern enum ictype installclass;

void
lex_begin();

void
lex_finish();

int
yylex_destroy();

#endif

