foo(int i, int j) ~ [
        setup: {
                i = [0?2];
                j = [0?2];
        }
]{
	int k, arr[2];
        arr[0] = 0;
        arr[i] = 2;
        arr[j] = 3;
        k = arr[0];
        k = arr[1];
}