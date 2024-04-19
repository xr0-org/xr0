#include <stdio.h>

int
main()
{
	int x;
	printf("x: %d\n", x);
}

struct list {
	int x;
	struct list *next;
};

void
foo()
{
	struct { char c; char d; } cd;

	struct point { int x; int y; } p;
	struct point p2;

	struct man { char *name; int age; };
	struct man john;

	struct list l1;
}
