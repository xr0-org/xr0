#include <stdlib.h>

void
unit(int limit) ~ [ setup: limit = [0?50]; ]
{
	int i;

	for (i = 0; (i < limit != 0) == 0; i++) ~ [ i = [0?limit+1]; ]
		;

	~ [ i == limit; ]
}

/*
!(i < limit)!=0

#2 >= #0

le(e1, e2) := { e1 <= e2 }

lt(e1, e2) := le(e1, e2-1)

eq(e1, e2) := le(e1, e2) ∪ le(e2, e1)
ne(e1, e2) :=
	(i)	lt(e1, e2)
	(ii)	lt(e2, e1)
*/
