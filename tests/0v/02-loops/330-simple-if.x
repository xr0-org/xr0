void
f()
{
	int i;

	for (i = 0; i < 1; i++) ~ [ i = [0?3]; if (i) i = 2; ]
		i = 1;

	~ [ 2 <= i; i <= 2; ]
}

/*
void
f()
{
	int i;

{
	i = 0;

.start:	~ [ i = [0?3]; if (i) i = 2; ]

	if (!(i < 1)) goto .end;

	i = 1;
	i++;

	goto .start;
.end: ;
}

	~ [ 2 <= i; i <= 2; ]
}
*/
