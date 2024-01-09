#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define true 1
#define false 0

char *
read_file(char *path) ~ [ .alloc result; ]
{
	FILE *f;
	char *str;
	int fsize; /* XXX: should be long */

	f = fopen(path, "rb");
	fseek(f, 0, SEEK_END);
	fsize = ftell(f);
	fseek(f, 0, SEEK_SET);  /* same as rewind(f); */
	str = malloc(fsize + 1);
	fread(str, fsize, 1, f);
	fclose(f);
	str[fsize] = '\0';
	return str;
}

int
main()
{
	char *file;
	/*struct lexer *l;*/

	file = read_file("/home/ubuntu/xr0/tests/3-program/100-lex/gen.l");

	/* XXX: temporarily framing #15
	l = parse(file);
	lexer_print(l);
	lexer_destroy(l);
	*/

	free(file);
}

struct pattern {
	char *name; char *pattern;
};

struct token {
	int isliteral;
	char *name; char *action;
};

struct pattern *
pattern_create(char *name, char *pattern) ~ [
	.alloc result;
	result->name = name;
	result->pattern = pattern;
]{
	struct pattern *p;

	p = malloc(sizeof(struct pattern));
	p->name = name;
	p->pattern = pattern;

	return p;
}

void
pattern_destroy(struct pattern *p) ~ [
	pre: p = pattern_create($, $);
	.dealloc p;
]{
	free(p);
}

struct token *
token_create(int isliteral, char *name, char *action) ~ [ .alloc result; ]
{
	struct token *tk;

	tk = malloc(sizeof(struct token));
	tk->isliteral = isliteral;
	tk->name = name;
	tk->action = action;

	return tk;
}

void
token_destroy(struct token *tk) ~ [
	pre: tk = token_create($, $, $);
	.dealloc tk;
]{
	free(tk);
}

struct lexer {
	char *pre; char *post;
	int npat; struct pattern *pattern;
	int ntok; struct token *token;
};

struct lexer *
lexer_create(char *pre, char *post, int npat, struct pattern *pattern,
		int ntok, struct token *token) ~ [
	.alloc result;
	result->pattern = pattern;
	result->token = token;
]{
	struct lexer *l;

	l = malloc(sizeof(struct lexer));
	l->pre = pre;
	l->post = post;
	l->pattern = pattern;
	l->npat = npat;
	l->token = token;
	l->ntok = ntok;

	return l;
}

void
lexer_destroy(struct lexer *l) ~ [
	pre: {
		l = lexer_create(
			$, $,
			$, malloc(1),
			$, malloc(1)
		);
	}

	.dealloc l->pattern;
	.dealloc l->token;
	.dealloc l;
]{
	free(l->pattern);
	free(l->token);
	free(l);
}

char *
substr(char *s, int n) ~ [ .alloc result; ]
{
	int len;
	char *ss;

	len = n + 1;
	ss = malloc(sizeof(char) * len);
	strncpy(ss, s, n);
	ss[n] = '\0';
	return ss;
}

char *
parse_id(char *input) ~ [ .alloc result; ]
{
	char *s;

	/*
	if (!isalpha(*input)) {
		fprintf(stderr, "id must begin with letter: '%s'", input);
		exit(1);
	}
	*/
	s = input + 1;
	/* XXX: '0' is a placeholder to allow this to parse */
	for (; isalpha(*s) || isdigit(*s) || *s == '_' ; 0) {
		s++;
	}
	return substr(input, s - input);
}

char *
skiplinespace(char *s) ~ [
	result = s; /* XXX */
]{
	for (; *s == ' ' || *s == '\t'; s++) {}
	return s;
}

#define KEYWORD_OPTION "%option"

char *
skipoptions(char *pos)
{
	char *id;

	if (strncmp(pos, KEYWORD_OPTION, strlen(KEYWORD_OPTION)) != 0) {
		return pos;
	}
	pos += strlen(KEYWORD_OPTION);
	pos = skiplinespace(pos);
	id = parse_id(pos);
	pos += strlen(id);
	free(id);
	return pos;
}

char *
parse_tonewline(char *input) ~ [ .alloc result; ]
{
	char *s;
	s = input; /* XXX: until loop is understood more */
	for (s = input; *s != '\n'; 0) {
		s++;
	}
	return substr(input, s - input);
}

struct patternresult {
	struct pattern *p;
	char *pos;
};

struct patternresult
parse_pattern(char *pos) ~ [
	result.p = pattern_create(malloc(1), malloc(1));
]{
	char *name; char *pattern;
	struct patternresult res;

	name = parse_id(pos);
	pos = pos + strlen(name);
	pos = skiplinespace(pos);
	pattern = parse_tonewline(pos);
	pos = pos + strlen(pattern); /* XXX: support pos += */

	res.p = pattern_create(name, pattern);
	res.pos = pos;
	return res;
}

struct patternet {
	struct pattern *pattern;
	int npat;
	char *pos;
};

char *
skipws(char *s)
{
	for (; isspace(*s); s++) {}
	return s;
}

int
compute_npat(char *pos)
{
	int n;
	struct patternresult r;

	n = 0;
	for (; strncmp(pos, "%%", 2) != 0; n++) {
		r = parse_pattern(pos);
		pos = skipws(r.pos);
		/* TODO: clean up r.p */
	}

	return n;
}

struct pattern *
parse_defs_n(char *pos, int npat) ~ [
	if (npat) {
		.alloc result;
	}
]{
	int i;
	struct pattern *p;
	struct patternresult parsed;

	if (npat) {
		p = malloc(npat);
		for (i = 0; i < npat; i++) {
			parsed = parse_pattern(pos);
			p[i] = *parsed.p;
			pos = skipws(parsed.pos);
		}
		return p;
	}

	return NULL;
}

struct patternet
parse_defsproper(char *pos) ~ [
	if (compute_npat(pos)) {
		.alloc result.pattern;
	}
]{
	struct patternet res;

	res.npat = compute_npat(pos);
	res.pattern = parse_defs_n(pos, res.npat);
	res.pos = pos;
	return res;
}

struct rulesresult {
	struct token *token;
	int ntok;
	char *pos;
};

struct tokenresult {
	struct token *tk;
	char *pos;
};

struct tokenresult
parse_token(char *pos);

int
compute_ntok(char *pos)
{
	int n;
	struct tokenresult r;

	n = 0;
	for (; *pos != '\0' && strncmp(pos, "%%", 2) != 0 ; n++) {
		r = parse_token(pos);
		pos = skipws(r.pos);
		/* TODO: clean up r.tk */
	}

	return n;
}

struct token *
parse_rules_n(char *pos, int ntok) ~ [
	if (ntok) {
		.alloc result;
	}
]{
	int i;
	struct token *t;
	struct tokenresult parsed;

	if (ntok) {
		t = malloc(ntok);
		for (i = 0; i < ntok; i++) {
			parsed = parse_token(pos);
			t[i] = *parsed.tk;
			pos = skipws(parsed.pos);
		}
		return t;
	}

	return NULL;
}

struct rulesresult
parse_rules(char *pos) ~ [
	if (compute_ntok(pos)) {
		.alloc result.token;
	}
]{
	struct rulesresult res;

	res.ntok = compute_ntok(pos);
	res.token = parse_rules_n(pos, res.ntok);
	res.pos = pos;
	return res;
}


struct lexer;

struct lexer *
parse(char *input);

void
lexer_destroy(struct lexer *);

void
lexer_print(struct lexer *);

void
pattern_print(struct pattern *);

void
token_print(struct token *);

void
lexer_print(struct lexer *l)
{
	int i;

	printf("\tpre: %s\n", l->pre);
	printf("\tpost: %s\n", l->post);
	printf("\tpatterns:\n");
	for (i = 0; i < l->npat; i++) {
		printf("\t\t");
		pattern_print(&l->pattern[i]);
		printf("\n");
	}
	printf("\ttokens:\n");
	for (i = 0; i < l->ntok; i++) {
		printf("\t\t");
		token_print(&l->token[i]);
		printf("\n");
	}
}

struct defsresult {
	char *pre;
	struct pattern *pattern;
	int npat;
	char *pos;
};

struct defsresult
parse_defs(char *pos);

struct rulesresult
parse_rules(char *pos);

char *
parse_toeof(char *input);

struct lexer *
parse(char *pos)
{
	struct defsresult def;
	struct rulesresult res;
	char *post;

	def = parse_defs(pos);
	pos = def.pos;
	if (strncmp(pos, "%%", 2) != 0) {
		fprintf(stderr, "invalid transition to rules: '%.*s'\n", 10,
			pos);
		exit(1);
	}
	pos = skipws(pos + 2); /* %% */
	res = parse_rules(pos);
	pos = res.pos;
	post = "";
	if (strncmp(pos, "%%", 2) == 0) {
		pos += 2;
		post = parse_toeof(pos);
	}
	return lexer_create(def.pre, post, def.npat, def.pattern, res.ntok,
		res.token);
}

struct stringresult {
	char *s;
	char *pos;
};

struct stringresult
parse_defsraw(char *input);

struct patternet
parse_defsproper(char *pos);

struct defsresult
parse_defs(char *pos)
{
	struct stringresult raw;
	struct patternet set;
	struct defsresult res;

	pos = skipws(pos);
	if (*pos == '\0') {
		fprintf(stderr, "EOF in defs\n");
		exit(1);
	}
	raw = parse_defsraw(pos);
	pos = raw.pos;
	pos = skipws(pos);
	pos = skipoptions(pos);
	pos = skipws(pos);
	set = parse_defsproper(pos);

	res.pre = raw.s;
	res.pattern = set.pattern;
	res.npat = set.npat;
	res.pos = set.pos;
	return res;
}

struct stringresult
parse_defsraw(char *input)
{
	char *pos;
	struct stringresult res;

	if (strncmp(input, "%{", 2) != 0) {
		res.s = "";
		res.pos = input;
		return res;
	}
	input += 2;
	pos = input;
	for (; strncmp(pos, "%}", 2) != 0; pos++) {}
	res.s = substr(input, pos - input);
	res.pos = pos + 2;
	return res;
}

void
pattern_print(struct pattern *p)
{
	printf("%s\t\t%s", p->name, p->pattern);
}


void
token_print(struct token *t)
{
	if (t->isliteral) {
		printf("\"%s\"", t->name);
	} else {
		printf("{%s}", t->name);
	}
	printf("\t\t%s", t->action);
}

struct tknameresult {
	int isliteral;
	char *name;
	char *pos;
};

struct tknameresult
parse_name(char *pos);

struct stringresult
parse_action(char *input);

struct tokenresult
parse_token(char *pos)
{
	struct tknameresult nameres;
	struct stringresult actionres;
	struct tokenresult res;

	nameres = parse_name(pos);
	actionres = parse_action(skiplinespace(nameres.pos));

	res.tk = token_create(nameres.isliteral, nameres.name, actionres.s);
	res.pos = actionres.pos;
	return res;
}

struct tknameresult
parse_token_id(char *pos);

struct tknameresult
parse_token_literal(char *input);

struct tknameresult
parse_token_pattern(char *pos);

struct tknameresult
parse_name(char *pos)
{
	switch (*pos) {
	case '{':
		return parse_token_id(pos);
	case '"':
		return parse_token_literal(pos);
	default:
		return parse_token_pattern(pos);
	}
}

struct tknameresult
parse_token_id(char *pos)
{
	char *id;
	struct tknameresult res;

	id = parse_id(++pos); /* skip '{' */
	pos += strlen(id);
	if (*pos != '}') {
		fprintf(stderr, "token id must end in '}' but has '%.*s\n'", 5,
			pos);
		exit(1);
	}

	res.isliteral = false;
	res.name = id;
	res.pos = pos + 1; /* '}' */
	return res;
}

struct tknameresult
parse_token_literal(char *input)
{
	char *pos;
	struct tknameresult res;

	input++; /* skip '"' */
	pos = input;
	for (pos++; *pos != '"'; pos++) {}

	res.isliteral = true;
	res.name = substr(input, pos - input);
	res.pos = pos + 1;
	return res;
}

struct tknameresult
parse_token_pattern(char *pos)
{
	char *id;
	struct tknameresult res;

	id = parse_id(pos);

	res.isliteral = true;
	res.name = id;
	res.pos = pos + strlen(id);
	return res;
}

struct stringresult
parse_action(char *input)
{
	char *pos;
	struct stringresult res;

	if (*input != '{') {
		fprintf(stderr, "action must begin with '{' but has '%.*s\n",
			10, input);
		exit(1);
	}
	input++; /* skip '{' */
	pos = input;
	for (; *pos != '}'; pos++) {}

	res.s = substr(input, pos - input);
	res.pos = pos + 1;
	return res;
}

char *
parse_toeof(char *input)
{
	char *s;

	for (s = input; *s != '\0'; 0) {
		s++;
	}
	return substr(input, s - input);
}
