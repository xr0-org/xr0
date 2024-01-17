#include <stdlib.h>
#include <stdio.h>

void
main();

void *
func2() {
	main();
	return NULL;
}

void *
main()
{
	func2();	
	return NULL;
}
