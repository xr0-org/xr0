#ifdef XR0

int
index() ~ [ return [0?50]; ];

#endif

int
index()
{	
	return 2;
}

int
main()
{
	int a[3];
	int i;
	int res;
	a[0] = 4;
	a[1] = 5;
	a[2] = 6;

	i = index();
	res = a[i];	/* ERROR: `i' could be out of bounds */
}
