void
foo()
{
	int i, j;
	i = 5;
	j = 4;
	
	#ifdef XR0
	~ [ i == 5; ]
	~ [ j == 4; ]
	#endif
}
