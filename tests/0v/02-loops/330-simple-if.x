void
f()
{
	int i;

	for (i = 0; i < 1; i++) ~ [ i = [0?3]; if (i) i = 2; ]
		i = 1;

	~ [ 2 <= i; i <= 2; ]
}

void
f()
{
	int i;

	i = 0;

l0 ~ [ i = [0?3]; if (i) i = 2; ]:

	if (!(i < 1)) goto l1;

	i = 1;
	i++;

	goto l0;

l1:
	~ [ 2 <= i; i <= 2; ]
}
