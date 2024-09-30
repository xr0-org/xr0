#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>

#define false 0
#define true 1

char *
read_file(char *path) ~ [
	char *file;

	file = malloc(1);
	*file = $;
	return file;
];

struct pattern {
	char *name; char *pattern;
};

struct pattern_arr {
	int n;
	struct pattern *p;
};

struct pattern_arr *
pattern_arr_create(int n, struct pattern *p) ~ [
	struct pattern_arr *arr;

	arr = malloc(sizeof(struct pattern));
	arr->n = n;
	arr->p = p;
	return arr;
]{
	struct pattern_arr *arr;

	arr = malloc(sizeof(struct pattern));
	arr->n = n;
	arr->p = p;
	return arr;
}

void
pattern_arr_destroy(struct pattern_arr *arr) ~ [
	setup: arr = pattern_arr_create($, malloc(1));
	free(arr->p);
	free(arr);
]{
	int i;

	for (i = 0; i < arr->n; i++) {
		free(arr->p[i].name);
		free(arr->p[i].pattern);
	}
	free(arr->p);
	free(arr);
}

void
pattern_print(struct pattern *p) ~ [
	setup: p = pattern_create(malloc(1), malloc(1));
];

struct token {
	int isliteral;
	char *name; char *action;
};

struct token_arr {
	int n;
	struct token *t;
};

struct token_arr *
token_arr_create(int n, struct token *t) ~ [
	struct token_arr *arr;

	arr = malloc(sizeof(struct token));
	arr->n = n;
	arr->t = t;
	return arr;
]{
	struct token_arr *arr;

	arr = malloc(sizeof(struct token));
	arr->n = n;
	arr->t = t;
	return arr;
}

void
token_arr_destroy(struct token_arr *arr) ~ [
	setup: arr = token_arr_create($, malloc(1));
	free(arr->t);
	free(arr);
]{
	int i;

	for (i = 0; i < arr->n; i++) {
		free(arr->t[i].name);
		free(arr->t[i].action);
	}
	free(arr->t);
	free(arr);
}

void
token_print(struct token *t) ~ [
	setup: t = token_create(0, "", "");
];



struct lexer {
	char *pre; char *post;
	struct pattern_arr *patterns;
	struct token_arr *tokens;
};

struct lexer *
lexer_create(char *pre, char *post, struct pattern_arr *patterns,
		struct token_arr *tokens) ~ [
	struct lexer *l;

	setup: {
		pre = .clump(1);
		post = .clump(1);
		patterns = pattern_arr_create($, malloc(1));
		tokens = token_arr_create($, malloc(1));
	};

	l = malloc(sizeof(struct lexer)); 
	l->pre = pre;
	l->post = post;
	l->patterns = patterns;
	l->tokens = tokens;
	return l;
];


int
beginsdefs(char *s);

int
isboundary(char *s);

struct stringresult {
	char *s;
	char *pos;
};

struct stringresult
parse_defsraw(char *input) ~ [
	struct stringresult res;
	res.s = malloc(1);
	res.pos = $;
	return res;
];

/* skipws: skip whitespace */
char *
skipws(char *s);

struct lexer *
parse(char *pos) ~ [
	char *pre; char *post;
	struct pattern_arr *patterns;
	struct token_arr *tokens;

	pre = malloc(1); *pre = $;
	patterns = pattern_arr_create($, malloc(1));
	tokens = token_arr_create($, malloc(1));
	post = malloc(1); *post = $;
	return lexer_create(pre, post, patterns, tokens);
];

void
lexer_destroy(struct lexer *l) ~ [
	setup: l = lexer_create(
		malloc(1), malloc(1),
		pattern_arr_create($, malloc(1)),
		token_arr_create($, malloc(1))
	);

	free(l->pre);
	free(l->post);
	pattern_arr_destroy(l->patterns);
	token_arr_destroy(l->tokens);
	free(l);
];

void
lexer_print(struct lexer *l) ~ [
	setup: l = lexer_create(
		malloc(1), malloc(1),
		pattern_arr_create($, malloc(1)),
		token_arr_create($, malloc(1))
	);
];

int
main(int argc, char **argv) ~ [
	setup: {
		argv = .clump(sizeof(char *) * 2);
		argv[1] = $;
	}
]{
	char *file;
	struct lexer *l;

	file = read_file(argv[1]);

	l = parse(file);
	lexer_print(l);
	lexer_destroy(l);
	free(file);
}

char *
read_file(char *path)
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

struct lexer *
lexer_create(char *pre, char *post, struct pattern_arr *patterns,
		struct token_arr *tokens)
{
	struct lexer *l;

	l = malloc(sizeof(struct lexer));
	l->pre = pre;
	l->post = post;
	l->patterns = patterns;
	l->tokens = tokens;
	return l;
}

void
lexer_destroy(struct lexer *l)
{
	int i;

	free(l->pre);
	free(l->post);
	pattern_arr_destroy(l->patterns);
	token_arr_destroy(l->tokens);
	free(l);
}

void
lexer_print(struct lexer *l)
{
	int i;

	puts("\tpre:");
	puts(l->pre);

	puts("\tpost:");
	puts(l->post);

	puts("\tpatterns:");
	for (i = 0; i < l->patterns->n; i++) {
		putchar('\t');
		putchar('\t');
		pattern_print(&l->patterns->p[i]);
		putchar('\n');
	}
	puts("\ttokens:");
	for (i = 0; i < l->tokens->n; i++) {
		putchar('\t');
		putchar('\t');
		token_print(&l->tokens->t[i]);
		putchar('\n');
	}
}

struct defsresult {
	char *pre;
	struct pattern_arr *patterns;
	char *pos;
};

char *
skipoptions(char *pos);

int
count_patterns(char *pos);

struct defsresult
parse_defs(char *pos) ~ [
	struct defsresult res;
	res.pre = malloc(sizeof(char) * 1000);
	res.patterns = pattern_arr_create($, malloc(1));
	res.pos = $;
	return res;
];

int
beginsdefs(char *s)
{
	return strncmp(s, "%{", 2) == 0;
}

struct rulesresult {
	struct token_arr *tokens;
	char *pos;
};

int
count_tokens(char *pos);

struct rulesresult
parse_rules(char *pos) ~ [
	struct rulesresult res;

	res.tokens = token_arr_create($, malloc(1));
	res.pos = $;
	return res;
];

char *
parse_toeof(char *input) ~ [ return malloc(sizeof(char) * 1000); ];

struct lexer *
parse(char *pos)
{
	struct defsresult defs;
	struct rulesresult rules;
	char *post;

	defs = parse_defs(pos);
	pos = defs.pos;
	if (!isboundary(pos)) {
		puts("invalid transition to rules");
		exit(1);
	}
	pos = skipws(pos + 2); /* %% */
	rules = parse_rules(pos);
	pos = rules.pos;
	post = parse_toeof(pos);
	return lexer_create(defs.pre, post, defs.patterns, rules.tokens);
}

int
isboundary(char *s)
{
	return strncmp(s, "%%", 2) == 0;
}

char *
substr(char *s, int n) ~ [ return malloc(sizeof(char) * 1000); ]
{
	int len;
	char *ss;

	len = n + 1;
	ss = malloc(sizeof(char) * len);
	strncpy(ss, s, len);
	ss[len-1] = '\0';
	return ss;
}

char *
skipws(char *s)
{
	for (; isspace(*s); s++) {}
	return s;
}

char *
skiplinespace(char *s)
{
	for (; *s == ' ' || *s == '\t'; s++) {}
	return s;
}

char *
parse_id(char *input) ~ [ return malloc(sizeof(char) * 1000); ];

char *
skipoptions(char *pos)
{
	char *keyword;
	char *id;

	pos = skipws(pos);
	keyword = "%option";
	if (strncmp(pos, keyword, strlen(keyword)) != 0) {
		return pos;
	}
	pos += strlen(keyword);
	pos = skiplinespace(pos);
	id = parse_id(pos);
	pos += strlen(id);
	free(id);
	return skipws(pos);
}

char *
parse_id(char *input)
{
	char *s;

	if (!isalpha(*input)) {
		/* TODO: print to stderr */
		puts("id must begin with letter");
		exit(1);
	}
	s = input + 1;
	/* XXX: '0' is a placeholder to allow this to parse */
	for (; isalpha(*s) || isdigit(*s) || *s == '_' ; 0) {
		s++;
	}
	return substr(input, s - input);
}

char *
parse_tonewline(char *input) ~ [ return malloc(sizeof(char) * 1000); ]
{
	char *s;
	s = input; /* must be here because not seen with skip loop hack */
	for (; *s != '\n'; 0) {
		s++;
	}
	return substr(input, s - input);
}

struct patternet {
	struct pattern_arr *patterns;
	char *pos;
};

struct patternet
parse_defsproper(char *input) ~ [
	struct patternet res;
	res.patterns = pattern_arr_create($, malloc(1));
	res.pos = $;
	return res;
];

struct defsresult
parse_defs(char *pos)
{
	struct stringresult raw;
	struct patternet set;
	struct defsresult res;

	pos = skipws(pos);
	if (*pos == '\0') {
		puts("EOF in defs");
		exit(1);
	}
	raw = parse_defsraw(pos);
	pos = raw.pos;
	pos = skipoptions(pos);
	set = parse_defsproper(pos);

	res.pre = raw.s;
	res.patterns = set.patterns;
	res.pos = set.pos;
	return res;
}

struct stringresult
parse_defsraw(char *input)
{
	char *pos;
	struct stringresult res;

	if (!beginsdefs(input)) {
		res.s = malloc(1);
		res.s[0] = '\0';
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

struct pattern *
pattern_create(char *name, char *pattern) ~ [	
	struct pattern *res;

	setup: {
		name = .clump(1);
		pattern = .clump(1);
	};

	res = malloc(sizeof(struct pattern));
	res->name = name;
	res->pattern = pattern;
	return res;
]{
	struct pattern *p;

	p = malloc(sizeof(struct pattern));
	p->name = name;
	p->pattern = pattern;
	return p;
}

void
pattern_print(struct pattern *p)
{
	puts("pattern:");
	puts(p->name);
	puts(p->pattern);
	puts("end pattern");
}


struct patternresult {
	struct pattern *p;
	char *pos;
};

struct patternpos {
	struct pattern *patterns; /* an array */
	char *pos;
};

struct patternpos
parse_defs_n(char *pos, int npat) ~ [
	struct patternpos res;

	res.patterns = malloc(sizeof(struct pattern) * npat);
	res.pos = $;
	return res;
];

struct patternet
parse_defsproper(char *input)
{
	int n;
	struct patternpos p_pos;
	struct patternet res;

	n = count_patterns(input);
	p_pos = parse_defs_n(input, n);
	res.patterns = pattern_arr_create(n, p_pos.patterns);
	res.pos = p_pos.pos;
	return res;
}

struct patternresult
parse_pattern(char *pos) ~ [
	struct patternresult res;

	res.p = pattern_create(malloc(1), malloc(1));
	return res;
];

int
count_patterns(char *pos)
{
	int n;
	struct patternresult parsed;

	n = 0;
	for (; strncmp(pos, "%%", 2) != 0; n++) {
		parsed = parse_pattern(pos);
		pos = skipws(parsed.pos);
		free(parsed.p->name);
		free(parsed.p->pattern);
		free(parsed.p);
	}

	return n;
}

struct patternresult
parse_pattern(char *pos)
{
	char *name; char *pattern;
	struct patternresult res;

	name = parse_id(pos);
	pos = pos + strlen(name);
	pos = skiplinespace(pos);
	pattern = parse_tonewline(pos);
	pos += strlen(pattern);

	res.p = pattern_create(name, pattern);
	res.pos = pos;
	return res;
}

struct patternpos
parse_defs_n(char *pos, int n)
{
	int i;
	struct pattern *p;
	struct patternresult parsed;
	struct patternpos res;

	p = malloc(sizeof(struct pattern) * n);
	for (i = 0; i < n; i++) {
		parsed = parse_pattern(pos);
		p[i] = *parsed.p;
		free(parsed.p);
		pos = skipws(parsed.pos);
	}

	res.patterns = p;
	res.pos = pos;
	return res;
}

struct token *
token_create(int isliteral, char *name, char *action) ~ [
	struct token *tk;

	tk = malloc(sizeof(struct token));
	tk->isliteral = isliteral;
	tk->name = name;
	tk->action = action;

	return tk;
]{
	struct token *tk;

	tk = malloc(sizeof(struct token));
	tk->isliteral = isliteral;
	tk->name = name;
	tk->action = action;

	return tk;
}

void
token_print(struct token *t)
{
	puts("token:");
	puts(t->name);
	puts(t->action);
	puts("end token");
}

struct tokenpos {
	struct token *tokens;
	char *pos;
};

struct tokenpos
parse_rules_n(char *pos, int ntok) ~ [
	struct tokenpos r;

	r.tokens = malloc(sizeof(struct token) * ntok);
	r.pos = $;
	return r;
];

struct rulesresult
parse_rules(char *pos)
{
	int n;
	struct rulesresult res;
	struct tokenpos rules_pos;

	n = count_tokens(pos);
	rules_pos = parse_rules_n(pos, n);
	res.tokens = token_arr_create(n, rules_pos.tokens);
	res.pos = rules_pos.pos;
	return res;
}

struct tokenresult {
	struct token *tk;
	char *pos;
};

struct tokenresult
parse_token(char *pos) ~ [
	struct tokenresult r;
	r.tk = token_create($, malloc($), malloc($));
	r.pos = $;
	return r;
];

int
count_tokens(char *pos)
{
	int n;
	struct tokenresult r;

	n = 0;
	for (; *pos != '\0' && strncmp(pos, "%%", 2) != 0 ; n++) {
		r = parse_token(pos);
		pos = skipws(r.pos);
		free(r.tk->name);
		free(r.tk->action);
		free(r.tk);
	}

	return n;
}

struct tokenpos
parse_rules_n(char *pos, int n)
{
	int i;
	struct token *t;
	struct tokenresult parsed;
	struct tokenpos res;

	t = malloc(sizeof(struct token) * n);
	for (i = 0; i < n; i++) {
		parsed = parse_token(pos);
		t[i] = *parsed.tk;
		free(parsed.tk);
		pos = skipws(parsed.pos);
	}
	res.tokens = t;
	res.pos = pos;
	return res;
}

struct tknameresult {
	int isliteral;
	char *name;
	char *pos;
};

struct tknameresult
parse_name(char *pos) ~ [
	struct tknameresult r;

	r.name = malloc(sizeof(char) * 1000);
	r.isliteral = $;
	r.pos = $;
	return r;
];

struct stringresult
parse_action(char *input) ~ [
	struct stringresult res;
	res.s = malloc(sizeof(char) * 1000);
	res.pos = $;
	return res;
];

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
parse_token_id(char *pos) ~ [
	struct tknameresult r;
	r.name = malloc(sizeof(char) * 1000);
	return r;
];

struct tknameresult
parse_token_literal(char *input) ~ [
	struct tknameresult r;

	r.name = malloc(sizeof(char) * 1000);
	return r;
];

struct tknameresult
parse_token_pattern(char *pos) ~ [
	struct tknameresult r;

	r.name = malloc(sizeof(char) * 1000);
	return r;
];

struct tknameresult
parse_name(char *pos)
{
	/* TODO: make into switch */
	if (*pos == '{') {
		return parse_token_id(pos);
	}
	if (*pos == '"') {
		return parse_token_literal(pos);
	}
	return parse_token_pattern(pos);
}

struct tknameresult
parse_token_id(char *pos)
{
	char *id;
	struct tknameresult res;

	id = parse_id(++pos); /* skip '{' */
	pos = pos + strlen(id);
	if (*pos != '}') {
		puts("token id must end in '}'");
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
		puts("action must begin with '{'");
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

	if (!isboundary(input)) {
		s = malloc(1);
		s[0] = '\0';
		return s;
	}

	s = input; /* must be here because not seen with skip loop hack */
	input += 2;
	for (; *s != '\0'; 0) {
		s++;
	}
	return substr(input, s - input);
}
