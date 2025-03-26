void
f()
{
	int i, k;

	for (i = 0; i < 1; i++) ~ [ i = [0?2]; if (i) k = 1; ]
		k = 1;

	~ [ 1 <= k; k <= 1; ]
}

void
f()
{
	int i, k;

	for (i = 0; i < 2; i++) ~ [
		i = [0?3];
		switch (i) {
		case 1: k = 0;
		case 2: k = 1;
		}
	]{
		k = i;
	}

	~ [ 1 <= k; k <= 1; ]
}

- show that invariant is initially satisfied. (potentially fragments invariant)
- for all fragments of the invariant we need to show that the execution of the
  loop body brings you to a state that satisfies at least one of the fragments.


- derive invariant: execute invariant body on the context and store all splits
- setup verify: show that the context satisfies the invariant (i.e. that it
  satisfies one of the invariant splits)
- execute loop: push the loop body and begin from each of the splits
	for each split:
		- if a loop point is encountered, verify the context at that
		  point satisfies the invariant (i.e. that it satisfies one of
		  the invariant splits)

		- if a break is encountered, jump to the end of the loop body
		  with the context as it is and proceed
