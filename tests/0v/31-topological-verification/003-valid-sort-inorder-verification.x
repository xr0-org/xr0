#include <stdlib.h>
#include <stdio.h>

#ifdef XR0

char *
bar() ~ [ return malloc(sizeof(char) * 10); ];

int *
foo() ~ [ return malloc(sizeof(int)); ];

#endif

int
main()
{
	int *i;
	char *s;

	i = foo();
	s = bar();
	free(s);
	free(i);
	return 0;
}

int *
foo()
{
	return malloc(sizeof(int));
}

int *
bar()
{
	return foo();
}
