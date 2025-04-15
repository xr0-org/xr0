void
f()
{
	int i;

	for (i = 0; i < 1; i++) ~ [ i = [0?3]; if (i) i = 2; ]
		i = 1;

	~ [ 2 <= i; i <= 2; ]
}
