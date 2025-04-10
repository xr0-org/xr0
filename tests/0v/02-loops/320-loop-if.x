f()
{
        int i, k;

        for (i = 0; i < 1; i++) ~ [ i = [0?2]; if (i) k = 1; ]
                k = 1;
        ~ [ 1 <= k; k <= 1; ]
}

f()
{
        int i, k;

        for (i = 0; i < 1; i++) ~ [
                int j;

                i = [0?2];
                for (j = 0; j < i; j++) k = 1;
        ]{
                k = 1;
        }
        ~ [ 1 <= k; k <= 1; ]
}

f()
{
        int i, k;

        for (i = 0; i < 1; i++) ~ [
                int j;

                i = [0?2];
                j = 0;
                while (1) {
                        if (j >= i)
                                break;
                        k = 1;
                        j++;
                }
        ]{
                k = 1;
        }
        ~ [ 1 <= k; k <= 1; ]
}
