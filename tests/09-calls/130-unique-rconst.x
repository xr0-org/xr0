int
f(int x) ~ [ return [0?2]; ]
{
	return 0;
}

int
g(int x, int y) ~ [
	setup: {
		x = [0?2];
		y = [0?2];
	}
]{
	int a; int b;

	if (x != y) {
		return 0;
	}

	a = f(x); /* "f:{($0, [?])}:0" */
	b = f(y); /* "f:{($1, [?])}:0" */

	~ [ a == b; ]
}
