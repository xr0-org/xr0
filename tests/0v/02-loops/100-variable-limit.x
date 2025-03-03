#include <stdlib.h>

void
unit(int limit) ~ [ setup: limit = [50?0]; ]
{
	int i;

	for (i = 0; i < limit; i++) ~ [ i = [0?limit+1]; ]
		;

	~ [ i == limit; ]
}

/*
limit = [0?50];

===

rconst:
	#0

	unreduced:
	⊢  INT_MIN <=       #0
	⊢       #0 <=  INT_MAX
	⊢        0 <=       #0
	⊢       #0 <=       49

	reduced:
	⊢        0 <=       #0
	⊢       #0 <=       49

stack:
	limit: <int:#0>

===

i = 0;

=== state prior to loop entry

rconst:
	#0

	⊢        0 <=       #0
	⊢       #0 <=       49

stack:
	limit:	<int:#0>
	i:	<int:0>

===

i = [0?limit+1];

=== invariant state

rconst:
	#0, #1

	⊢        0 <=       #0
	⊢       #0 <=       49
	⊢        0 <=       #1
	⊢       #1 <=       #0 

stack:
	limit:	<int:#0>
	i:	<int:#1>

===

if (i < limit)
	break;

=== ⊢ i < limit

rconst:
	#0, #1

	unreduced:
	⊢        0 <=       #0
	⊢       #0 <=       49
	⊢        0 <=       #1
	⊢       #1 <=       #0 
	⊢       #1 <=   #0 - 1 

	reduced:
	⊢        1 <=       #0
	⊢       #0 <=       49
	⊢        0 <=       #1
	⊢       #1 <=   #0 - 1 

=== ⊢ !(i < limit)

rconst:
	#0, #1

	unreduced:
	⊢        0 <=       #0
	⊢       #0 <=       49
	⊢        0 <=       #1
	⊢       #1 <=       #0 
	⊢       #0 <=	    #1

	reduced:
	⊢        0 <=       #0
	⊢       #0 <=       49
	⊢       #0 <=	    #1
	⊢       #1 <=       #0 
*/
