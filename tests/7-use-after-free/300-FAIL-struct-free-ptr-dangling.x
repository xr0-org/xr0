#include <stdlib.h>
#include <stdio.h>

struct report {
	int n;
	struct score **scores;
};

struct score {
	char *subject;
	int grade;
};

struct score *
create_score(char *subject, int grade) ~ [
	struct score *s;

	pre: subject = .alloc(sizeof(char *) * 100);

	s = .alloc(sizeof(struct score));
	s->subject = subject;
	s->grade = grade;
	return s;
] {
	struct score *s;
	s = malloc(sizeof(struct score));

	s->subject = subject;
	s->grade = grade;

	return s;
}

struct report *
create_report()
{
	struct score *s;
	char *sub;

	sub = malloc(sizeof(char) * 7);
	*sub = "english";

	s = create_score(sub, 1);
	free(sub);		/* dangling ptr */
	puts(s->subject);
}
