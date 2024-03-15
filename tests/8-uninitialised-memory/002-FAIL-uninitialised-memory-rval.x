void
uninitialised_memory2()
{
	int p;
	int *q;
	int r;

	q = &p;
	r = *q;		/* ERROR: unjustified indirection */
}
