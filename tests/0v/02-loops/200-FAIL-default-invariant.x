#include <stdlib.h>

void
unit()
{
	int i;

	for (i = 0; i < 9; i++) ~ [] /* asserts invariance of i == 0 */ 
		;
}
