#include <stdlib.h>

int *
dangling_static_return(int *i) ~ [

] {
	static int p;
	
	p = 5;
	return &p;	/* fine since p is declared as static */
}
