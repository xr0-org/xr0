#ifndef XR0_LEX_H
#define XR0_LEX_H

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

struct lexememarker *
lexememarker_copy(struct lexememarker *);

void
lexememarker_destroy(struct lexememarker *);

char *
lexememarker_str(struct lexememarker *);

int
lexememarker_line(struct lexememarker *);

extern int verbose;
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

