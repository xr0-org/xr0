#include <stdlib.h>

void
unit(int limit) ~ [ setup: limit = [0?50]; ]
{
	int i;

	for (i = 0; i < limit; i++) ~ [ i = [0?limit]; ]
		;

	~ [ i == limit; ]
}


/*

in impl:
#1-#0 <= - 1 
<=> #1 <= #0 - 1  (a)

#5-#1 <= 1
<=> #5 <= #1 + 1  (b)

(a) and (b) imply
	#5 <= #1 + 1 <= (#0 - 1) + 1 == #0
i.e. #5 == #0 is feasible

in spec:
#5-#0 <= -1
<=>  #5 <= #0 - 1

 */
