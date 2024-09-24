int
foo(int x) ~ [ setup: x = [0?2]; ]
{
	return 3;
}

int
bar(int x)
{
	foo(1);
	return 0;
}
