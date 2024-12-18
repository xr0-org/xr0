D			[0-9]
L			[a-zA-Z_]
H			[a-fA-F0-9]
E			[Ee][+-]?{D}+
FS			(f|F|l|L)
IS			(u|U|l|L)*
%{
#include <stdio.h>
#include <ctype.h>
#include <assert.h>
#include "lex.h"
#include "util.h"
#include "gram_util.h"
#include "gram.tab.h"

void
count(void);

void
comment(void);

void
preproc(void);

long
check_type(void);
%}

%%

%{
	extern int LEX_START_TOKEN;
	if (LEX_START_TOKEN) {
		int t = LEX_START_TOKEN;
		LEX_START_TOKEN = 0;
		return t;
	}
%}


"#"			{ preproc(); }
"/*"			{ comment(); }
".malloc"		{ count(); return(TK_ALLOC); }
".free"			{ count(); return(TK_DEALLOC); }
".clump"		{ count(); return(TK_CLUMP); }

"auto"			{ count(); return(AUTO); }
"axiom"			{ count(); return(AXIOM); }
"break"			{ count(); return(BREAK); }
"case"			{ count(); return(CASE); }
"char"			{ count(); return(CHAR); }
"const"			{ count(); return(CONST); }
"continue"		{ count(); return(CONTINUE); }
"default"		{ count(); return(DEFAULT); }
"do"			{ count(); return(DO); }
"double"		{ count(); return(DOUBLE); }
"else"			{ count(); return(ELSE); }
"enum"			{ count(); return(ENUM); }
"extern"		{ count(); return(EXTERN); }
"float"			{ count(); return(FLOAT); }
"for"			{ count(); return(FOR); }
"goto"			{ count(); return(GOTO); }
"if"			{ count(); return(IF); }
"int"			{ count(); return(INT); }
"lemma"			{ count(); return(LEMMA); }
"long"			{ count(); return(LONG); }
"register"		{ count(); return(REGISTER); }
"return"		{ count(); return(RETURN); }
"short"			{ count(); return(SHORT); }
"signed"		{ count(); return(SIGNED); }
"sizeof"		{ count(); return(SIZEOF); }
"some"			{ count(); return(SOME); }
"static"		{ count(); return(STATIC); }
"sfunc"			{ count(); return(SFUNC); }
"struct"		{ count(); return(STRUCT); }
"switch"		{ count(); return(SWITCH); }
"typedef"		{ count(); return(TYPEDEF); }
"union"			{ count(); return(UNION); }
"unsigned"		{ count(); return(UNSIGNED); }
"void"			{ count(); return(VOID); }
"volatile"		{ count(); return(VOLATILE); }
"while"			{ count(); return(WHILE); }

{L}({L}|{D})*		{ count(); return(check_type()); }

0[xX]{H}+{IS}?		{ count(); return(CONSTANT); }
0{D}+{IS}?		{ count(); return(CONSTANT); }
{D}+{IS}?		{ count(); return(CONSTANT); }
L?'(\\.|[^\\'])+'	{ count(); return(CHAR_LITERAL); }

{D}+{E}{FS}?		{ count(); return(CONSTANT); }
{D}*"."{D}+({E})?{FS}?	{ count(); return(CONSTANT); }
{D}+"."{D}*({E})?{FS}?	{ count(); return(CONSTANT); }

L?\"(\\.|[^\\"])*\"	{ count(); return(STRING_LITERAL); }

"..."			{ count(); return(ELLIPSIS); }
">>="			{ count(); return(RIGHT_ASSIGN); }
"<<="			{ count(); return(LEFT_ASSIGN); }
"+="			{ count(); return(ADD_ASSIGN); }
"-="			{ count(); return(SUB_ASSIGN); }
"*="			{ count(); return(MUL_ASSIGN); }
"/="			{ count(); return(DIV_ASSIGN); }
"%="			{ count(); return(MOD_ASSIGN); }
"&="			{ count(); return(AND_ASSIGN); }
"^="			{ count(); return(XOR_ASSIGN); }
"|="			{ count(); return(OR_ASSIGN); }
">>"			{ count(); return(RIGHT_OP); }
"<<"			{ count(); return(LEFT_OP); }
"++"			{ count(); return(INC_OP); }
"--"			{ count(); return(DEC_OP); }
"->"			{ count(); return(PTR_OP); }
"&&"			{ count(); return(AND_OP); }
"||"			{ count(); return(OR_OP); }
"<="			{ count(); return(LE_OP); }
">="			{ count(); return(GE_OP); }
"=="			{ count(); return(EQ_OP); }
"!="			{ count(); return(NE_OP); }
";"			{ count(); return(';'); }
("{"|"<%")		{ count(); return('{'); }
("}"|"%>")		{ count(); return('}'); }
","			{ count(); return(','); }
":"			{ count(); return(':'); }
"="			{ count(); return('='); }
"("			{ count(); return('('); }
")"			{ count(); return(')'); }
("["|"<:")		{ count(); return('['); }
("]"|":>")		{ count(); return(']'); }
"."			{ count(); return('.'); }
"&"			{ count(); return('&'); }
"!"			{ count(); return('!'); }
"~"			{ count(); return('~'); }
"-"			{ count(); return('-'); }
"+"			{ count(); return('+'); }
"*"			{ count(); return('*'); }
"/"			{ count(); return('/'); }
"%"			{ count(); return('%'); }
"<"			{ count(); return('<'); }
">"			{ count(); return('>'); }
"^"			{ count(); return('^'); }
"|"			{ count(); return('|'); }
"?"			{ count(); return('?'); }
"@"			{ count(); return(ISDEALLOCAND_OP); }
"$"			{ count(); return(ARB_ARG); }

[ \t\v\n\f]		{ count(); }
.			{ /* ignore bad characters */ }

%%

int
yywrap(void)
{
	return 1;
}

void
comment(void)
{
	char c, c1;

	while ((c = input()) != '*' && c != EOF) {
		putchar(c);
	}

	if ((c1 = input()) != '/' && c != EOF) {
		unput(c1);
		comment();
		return;
	}

	if (c != EOF) {
		putchar(c1);
	}
}

struct lexememarker marker;

struct lexememarker *
lexloc(void)
{
	assert(marker.filename);
	return lexememarker_copy(&marker);
}

struct lexememarker
process_linemarker(void)
{
	/* advance beyond '#' */
	char c = input();
	if (!isspace(c)) {
		fprintf(stderr, "expected space before line number\n");
		exit(EXIT_FAILURE);
	}
	/* skip ws ahead of linenum */
	while (isspace(c)) {
		c = input();
	}
	if (!isdigit(c)) {
		fprintf(stderr, "expected line number in line marker\n");
		exit(EXIT_FAILURE);
	}
	/* parse linenum */
	int linenum = 0;
	while (isdigit(c)) {
		linenum *= 10;
		linenum += c - '0';
		c = input();
	}
	if (!isspace(c)) {
		fprintf(stderr, "expected space before file name\n");
		exit(EXIT_FAILURE);
	}
	/* skip ws ahead of linenum */
	while (isspace(c)) {
		c = input();
	}
	if (c != '"') {
		fprintf(stderr, "file name must begin with opening quote\n");
		exit(EXIT_FAILURE);
	}
	struct strbuilder *b = strbuilder_create();
	while ((c = input()) != '"') {
		strbuilder_putc(b, c);
	}
	char *name = strbuilder_build(b);
	c = input(); /* advance past closing quote */
	if (!isspace(c)) {
		fprintf(stderr, "expected space before flags\n");
		exit(EXIT_FAILURE);
	}
	int flags = 0;
	while (c != '\n') {
		if (isdigit(c)) {
			switch (c) {
			case '1':
				flags |= LM_FLAG_NEW_FILE;
				break;
			case '2':
				flags |= LM_FLAG_RESUME_FILE;
				break;
			case '3':
				flags |= LM_FLAG_SYS_HEADER;
				break;
			case '4':
				flags |= LM_FLAG_IMPLICIT_EXTERN;
				break;
			default:
				fprintf(stderr, "invalid flag `%c'\n", c);
				exit(EXIT_FAILURE);
			}
		}
		c = input();
	}
	return (struct lexememarker) {
		.linenum = linenum,
		.column = 0,
		.filename = name,
		.flags = flags,
	};
}

void
preproc(void)
{	
	if (marker.filename) {
		free(marker.filename);
	}
	marker = process_linemarker();
}

void
count(void)
{
	for (char *s = yytext; *s; s++) {
		switch (*s) {
		case '\n':
			marker.column = 0;
			marker.linenum++;
			break;
		case '\t':
			marker.column += 8 - (marker.column % 8);
			break;
		default:
			marker.column++;
			break;
		}
	}
}

long
check_type(void)
{
	if (map_get(table, yytext)) {
		return TYPE_NAME;
	}
	switch (installclass) {
	case IC_TYPE:
		map_set(table, dynamic_str(yytext), (void *) true);
		installclass = IC_NONE;
		break;
	default:
		break;
	}
	return IDENTIFIER;
}

struct map *table;
enum ictype installclass;

void
lex_begin(void)
{
	table = map_create();
	installclass = IC_NONE;
	marker = (struct lexememarker) {
		.linenum	= 0,
		.column		= 0,
		.filename	= NULL,
		.flags		= 0,
	};
}

void
lex_finish(void)
{
	if (marker.filename) {
		free(marker.filename);
	}
	map_destroy(table);
}

static char *
strip_iflibx(char *filename);

struct lexememarker *
lexememarker_create(int linenum, int column, char *filename,
		enum linemarker_flag flags)
{
	struct lexememarker *loc = malloc(sizeof(struct lexememarker));
	loc->linenum = linenum;
	loc->column = column;
	loc->filename = strip_iflibx(filename);
	loc->flags = flags;
	return loc;
}

static char *
strip_iflibx(char *filename)
{
	int includes_len = strlen(XR0_INCLUDES_SEGMENT);

	char *libx_start = strstr(filename, XR0_INCLUDES_SEGMENT);
	if (!libx_start) {
		return filename;
	}
	int len = (strlen(libx_start) - includes_len) + 1;
	char *s = malloc(sizeof(char) * len);
	strcpy(s, libx_start + includes_len);
	free(filename);
	return s;
}

struct lexememarker *
lexememarker_copy(struct lexememarker *loc)
{
	assert(loc);
	return lexememarker_create(
		loc->linenum,
		loc->column,
		dynamic_str(loc->filename),
		loc->flags
	);
}

void
lexememarker_destroy(struct lexememarker *loc)
{
	free(loc->filename);
	free(loc);
}

int
lexememarker_linenum(struct lexememarker *loc)
{
	return loc->linenum;
}

char *
lexememarker_filename(struct lexememarker *loc)
{
	return loc->filename;
}

char *
lexememarker_str(struct lexememarker *loc)
{
	struct strbuilder *b = strbuilder_create();
	strbuilder_printf(b, "%s:%d:%d", loc->filename, loc->linenum, loc->column);
	return strbuilder_build(b);
}
