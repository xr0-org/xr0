int
index()
{	
	return 2;
}

int
main()
{
	int a[4];
	int i;
	int res;
	a[0] = 4;
	a[1] = 5;
	a[2] = 6;
	a[3] = 7;

	i = index();
	if (i < 0) {
		return 0;
	}
	if (i > 3) {
		return 0;
	}
	res = a[i];
	~ [ res == a[i]; ] 
}
