#include <stdlib.h>
#include <stdio.h>

void
main();

void *
func2()
{
	main();
	return NULL;
}

void *
func3();

void *
main()
{
	func3();
	return NULL;
}

void *
func3()
{
	return func2();
}
