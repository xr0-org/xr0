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

#ifdef XR0

struct score *
create_score(char *subject, int grade) ~ [
	struct score *s;

	setup: subject = .malloc(sizeof(char *) * 100);

	s = .malloc(sizeof(struct score));
	s->subject = subject;
	s->grade = grade;
	return s;
];

#endif

struct score *
create_score(char *subject, int grade)
{
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
