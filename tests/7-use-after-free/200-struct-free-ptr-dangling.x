#include <stdlib.h>

struct report {
	int n;
	struct score **scores;
};

struct score {
	char *subject;
	char grade;
};

struct score *
create_score(char *subject, char grade)
{
	struct score *s = malloc(sizeof(struct score));

	s->subject = subject;
	s->grade = grade;

	return s;
}

struct report *
create_report()
{
	char *lang = malloc(sizeof(char) * 7);
	*lang = "english";

	struct score *s = create_score(lang, 'A');

	free(english);		/* dangling ptr */

	*s->subject = "math";	/* ERROR: unjustified indirection */
}
