int
foo(int x) ~ [ setup: x = [0?2]; ]
{
	return 3;
}

int
bar(int x)
{
	foo(2); /* FAIL */
	return 0;
}