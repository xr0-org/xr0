void
undefined_memory2()
{
	int p;
	int *q;
	int r;

	q = &p;
	r = *q;		/* ERROR: undefined indirection */
}
