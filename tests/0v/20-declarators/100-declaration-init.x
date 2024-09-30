void
foo()
{
	int i = 5, j = 4;

	#ifdef XR0
	~ [ i == 5; j == 4; ]
	#endif
}
