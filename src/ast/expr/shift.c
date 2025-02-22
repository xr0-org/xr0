#include <stdlib.h>

struct shift {
	char *id;
	int width;
};

struct shift *
shift_create(char *id, int width)
{
	struct shift *s = malloc(sizeof(struct shift));
	s->id = id;
	s->width = width;
	return s;
}

void
shift_destroy(struct shift *s)
{
	free(s->id);
	free(s);
}

char *
shift_id(struct shift *s) { return s->id; }

int
shift_width(struct shift *s) { return s->width; }

