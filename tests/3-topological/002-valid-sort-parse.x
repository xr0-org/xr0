#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define false 0
#define true 1

char *
read_file(char *path) ~ [ return .alloc(1); ];

struct lexer;

struct lexer *
parse(char *input) ~ [
	struct lexer *l;
	l = lexer_create(
		$, $,
		$, malloc(1),
		$, malloc(1)
	);
	return l;
];

void
lexer_destroy(struct lexer *l) ~ [
	pre: {
		l = lexer_create(
			$, $,
			$, malloc(1),
			$, malloc(1)
		);
	}
	.dealloc(l->pattern);
	.dealloc(l->token);
	.dealloc(l);
];

void
lexer_print(struct lexer *l) ~ [
	pre: {
		l = lexer_create(
			.clump(1), .clump(1),
			$, .alloc(1),
			$, .alloc(1)
		);
	}
];

int
main()
{
	char *file;
	struct lexer *l;

	file = read_file("tests/3-program/100-lex/gen.l");

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

struct pattern {
	char *name; char *pattern;
};

void
pattern_print(struct pattern *p) ~ [
	pre: p = pattern_create("", "");
];

struct token {
	int isliteral;
	char *name; char *action;
};

void
token_print(struct token *t) ~ [
	pre: t = token_create(0, "", "");
];

struct lexer {
	char *pre; char *post;
	int npat; struct pattern *pattern;
	int ntok; struct token *token;
};

struct lexer *
lexer_create(char *pre, char *post, int npat, struct pattern *pattern,
		int ntok, struct token *token) ~ [
	struct lexer *l;
	l = .alloc(sizeof(struct lexer)); 
	l->pre = pre;
	l->post = post;
	l->pattern = pattern;
	l->token = token;
	return l;
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
lexer_destroy(struct lexer *l)
{
	free(l->pattern);
	free(l->token);
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
	for (i = 0; i < l->npat; i++) {
		putchar('\t');
		putchar('\t');
		pattern_print(&l->pattern[i]);
		putchar('\n');
	}
	puts("\ttokens:");
	for (i = 0; i < l->ntok; i++) {
		putchar('\t');
		putchar('\t');
		token_print(&l->token[i]);
		putchar('\n');
	}
}

/* skipws: skip whitespace */
char *
skipws(char *s);

struct defsresult {
	char *pre;
	struct pattern *pattern;
	int npat;
	char *pos;
};

int
beginsdefs(char *s);

int
count_patterns(char *pos);

struct defsresult
parse_defs(char *input) ~ [
	struct defsresult res;
	if (beginsdefs(skipws(input))) {
		res.pre = .alloc(1);
	}
	if (count_patterns(input)) {
		res.pattern = .alloc(1);
	}
	return res;
];

int
beginsdefs(char *s)
{
	return strncmp(s, "%{", 2) == 0;
}

struct rulesresult {
	struct token *token;
	int ntok;
	char *pos;
};

struct rulesresult
parse_rules(char *pos) ~ [
	struct rulesresult res;
	res.token = $; /* TODO: put in else block */
	if (count_tokens(pos)) {
		res.token = .alloc(sizeof(struct token));
	}
	res.pos = $;
	return res;
];

char *
parse_toeof(char *input) ~ [ return .alloc(1); ];

struct lexer *
parse(char *pos)
{
	struct defsresult def;
	struct rulesresult res;
	char *post;

	def = parse_defs(pos);
	pos = def.pos;
	if (strncmp(pos, "%%", 2) != 0) {
		/* TODO: fprintf breaks */
		/*fprintf(stderr, "invalid transition to rules: '%.*s'\n", 10, pos);*/
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

char *
substr(char *s, int n) ~ [ return .alloc(1); ]
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
parse_id(char *input) ~ [ return .alloc(1); ];

char *
skipoptions(char *pos)
{
	char *keyword;
	char *id;

	keyword = "%option";
	if (strncmp(pos, keyword, strlen(keyword)) != 0) {
		return pos;
	}
	pos += strlen(keyword);
	pos = skiplinespace(pos);
	id = parse_id(pos);
	pos += strlen(id);
	free(id);
	return pos;
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
parse_tonewline(char *input) ~ [ return .alloc(1); ]
{
	char *s;
	s = input; /* must be here because not seen with skip loop hack */
	for (; *s != '\n'; 0) {
		s++;
	}
	return substr(input, s - input);
}

struct stringresult {
	char *s;
	char *pos;
};

struct stringresult
parse_defsraw(char *input) ~ [
	struct stringresult res;

	res.s = $;
	if (beginsdefs(input)) {
		res.s = .alloc(1);
	}
	res.pos = $;
	return res;
];

struct patternet {
	struct pattern *pattern;
	int npat;
	char *pos;
};

struct patternet
parse_defsproper(char *input) ~ [
	struct patternet res;
	res.pattern = $; /* TODO: put in else block */
	if (count_patterns(input)) {
		res.pattern = .alloc(1);
	}
	res.npat = $;
	res.pos = $;
	return res;
];

struct defsresult
parse_defs(char *input)
{
	struct stringresult raw;
	struct patternet set;
	struct defsresult res;

	input = skipws(input);
	if (*input == '\0') {
		puts("EOF in defs");
		exit(1);
	}
	raw = parse_defsraw(input);
	input = raw.pos;
	input = skipws(input);
	input = skipoptions(input);
	input = skipws(input);
	set = parse_defsproper(input);

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

	if (!beginsdefs(input)) {
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

struct pattern *
pattern_create(char *name, char *pattern) ~ [
	struct pattern p;
	p = .alloc(sizeof(struct pattern));
	p->name = name;
	p->pattern = pattern;
	return p;
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
	struct pattern *p;
	char *pos;
};

struct patternpos
parse_defs_n(char *pos, int npat) ~ [
	struct patternpos res;
	res.p = $; /* TODO: put in else block */
	if (npat) {
		res.p = .alloc(1);
	}
	res.pos = $;
	return res;
];

struct patternet
parse_defsproper(char *input)
{
	struct patternet res;
	struct patternpos defs_n;

	res.npat = count_patterns(input);
	defs_n = parse_defs_n(input, res.npat);
	res.pattern = defs_n.p;
	res.pos = defs_n.pos;
	return res;
}

struct patternresult
parse_pattern(char *pos) ~ [
	result.p = pattern_create(malloc(1), malloc(1));
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
		/* TODO: clean up r.p */
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
parse_defs_n(char *pos, int npat)
{
	int i;
	struct pattern *p;
	struct patternresult parsed;
	struct patternpos res;

	p = NULL;
	if (npat) {
		p = malloc(sizeof(struct pattern) * npat);
		for (i = 0; i < npat; i++) {
			parsed = parse_pattern(pos);
			p[i] = *parsed.p;
			pos = skipws(parsed.pos);
		}
	}

	res.p = p;
	res.pos = pos;
	return res;
}

struct token *
token_create(int isliteral, char *name, char *action) ~ [
	struct token *tk;
	tk = .alloc(sizeof(struct token));
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

int
count_tokens(char *pos);

struct tokenpos {
	struct token *t;
	char *pos;
};

struct tokenpos
parse_rules_n(char *pos, int ntok) ~ [
	struct tokenpos tpos;
	tpos.t = $; /* TODO: put in else block */
	if (ntok) {
		tpos.t = .alloc(1);
	}
	tpos.pos = $;
	return tpos;
];

struct rulesresult
parse_rules(char *pos)
{
	struct rulesresult res;
	struct tokenpos rules_n;

	res.ntok = count_tokens(pos);
	rules_n = parse_rules_n(pos, res.ntok);
	res.token = rules_n.t;
	res.pos = rules_n.pos;
	return res;
}

struct tokenresult {
	struct token *tk;
	char *pos;
};

struct tokenresult
parse_token(char *pos) ~ [
	struct tokenresult tres;
	tres.tk = token_create($, malloc($), malloc($));
	tres.pos = $;
	return tres;
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
		/* TODO: clean up r.tk */
	}

	return n;
}

struct tokenpos
parse_rules_n(char *pos, int ntok)
{
	int i;
	struct token *t;
	struct tokenresult parsed;
	struct tokenpos res;

	t = NULL;
	if (ntok) {
		t = malloc(sizeof(struct token) * ntok);
		for (i = 0; i < ntok; i++) {
			parsed = parse_token(pos);
			t[i] = *parsed.tk;
			pos = skipws(parsed.pos);
		}
	}

	res.t = t;
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
	struct tknameresult res;
	res.name = .alloc(1);
	res.isliteral = $;
	res.pos = $;
	return res;
];

struct stringresult
parse_action(char *input) ~ [
	struct stringresult res;
	res.s = .alloc(1);
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
	struct tknameresult res;
	res.name = .alloc(1);
	return res;
];

struct tknameresult
parse_token_literal(char *input) ~ [
	struct tknameresult res;
	res.name = .alloc(1);
	return res;
];

struct tknameresult
parse_token_pattern(char *pos) ~ [
	struct tknameresult res;
	res.name = .alloc(1);
	return res;
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
	pos += strlen(id);
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

	s = input; /* must be here because not seen with skip loop hack */
	for (; *s != '\0'; 0) {
		s++;
	}
	return substr(input, s - input);
}
