#include <stdio.h>
#include <stdlib.h>

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

struct lexer;

struct lexer *
parse(char *input);

void
lexer_destroy(struct lexer *);

void
lexer_print(struct lexer *);

int
main()
{
	char *file;
	struct lexer *l;

	file = read_file("gen.l");

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

void
pattern_print(struct pattern *);

struct token {
	int isliteral;
	char *name; char *action;
};

void
token_print(struct token *);

struct lexer {
	char *pre; char *post;
	int npat; struct pattern *pattern;
	int ntok; struct token *token;
};

struct lexer *
lexer_create(char *pre, char *post, int npat, struct pattern *pattern,
		int ntok, struct token *token)
{
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

/* skipws: skip whitespace */
char *
skipws(char *s);

struct defsresult {
	char *pre;
	struct pattern *pattern;
	int npat;
	char *pos;
};

struct defsresult
parse_defs(char *pos);

struct rulesresult {
	struct token *token;
	int ntok;
	char *pos;
};

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

char *
substr(char *s, int n)
{
	int len;
	char *ss;

	len = n + 1;
	ss = malloc(sizeof(char) * len);
	snprintf(ss, len, "%s", s);
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
parse_id(char *input);

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
		fprintf(stderr, "id must begin with letter: '%s'", input);
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
parse_tonewline(char *input)
{
	char *s;
	for (s = input; *s != '\n'; 0) {
		s++;
	}
	return substr(input, s - input);
}

struct stringresult {
	char *s;
	char *pos;
};

struct stringresult
parse_defsraw(char *input);

struct patternet {
	struct pattern *pattern;
	int npat;
	char *pos;
};

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
	res.pattern = set.patern;
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

struct pattern *
pattern_create(char *name, char *pattern)
{
	struct pattern *p;

	p = malloc(sizeof(struct pattern));
	p->name = name;
	p->pattern = pattern;

	return p;
}

void
pattern_print(struct pattern *p)
{
	printf("%s\t\t%s", p->name, p->pattern);
}


struct patternresult {
	struct pattern *p;
	char *pos;
};

struct patternresult
parse_pattern(char *pos);

struct patternet
parse_defsproper(char *pos)
{
	int npat;
	struct pattern *pattern;
	struct patternresult parsed;
	struct patternet res;

	npat = 0;
	pattern = NULL;
	for (; strncmp(pos, "%%", 2) != 0 ; npat++) {
		parsed = parse_pattern(pos);
		pos = parsed.pos;
		pattern = realloc(pattern, sizeof(struct pattern) * (npat + 1));
		pattern[npat] = *parsed.p;
		pos = skipws(pos);
	}
	res.pattern = pattern;
	res.npat = npat;
	res.pos = pos;
	return pos;
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

struct token *
token_create(int isliteral, char *name, char *action)
{
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
	if (t->isliteral) {
		printf("\"%s\"", t->name);
	} else {
		printf("{%s}", t->name);
	}
	printf("\t\t%s", t->action);
}

struct tokenresult {
	struct token *tk;
	char *pos;
};

struct tokenresult
parse_token(char *pos);

struct rulesresult
parse_rules(char *pos)
{
	int ntok;
	struct token *token;
	struct tokenresult parsed;
	struct rulesresult res;

	ntok = 0;
	token = NULL;
	for (; *pos != '\0' && strncmp(pos, "%%", 2) != 0 ; ntok++) {
		parsed = parse_token(pos);
		pos = parsed.pos;
		token = realloc(token, sizeof(struct token) * (ntok + 1));
		token[ntok] = *parsed.tk;
		pos = skipws(pos);
	}
	res.token = token;
	res.ntok = ntok;
	res.pos = pos;
	return res;
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
