int
f(int x) ~ [ return [0?2]; ]
{
	return 0;
}

int
g()
{
	int a; int b;

	a = f(0);
	b = f(0);

	~ [ a == b; ]
}
