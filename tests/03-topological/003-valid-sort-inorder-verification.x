#include <stdlib.h>
#include <stdio.h>

char *
bar() ~ [ return malloc(sizeof(char) * 10); ];

int *
foo() ~ [ return malloc(sizeof(int)); ];

int
main()
{
	int *i;
	char *s;

	i = foo();
	s = bar();
	free(s);
	free(i);
}

int
foo()
{
	return malloc(sizeof(int));
}

int
bar()
{
	return foo();
}
