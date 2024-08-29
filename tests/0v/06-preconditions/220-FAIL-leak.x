#include <stdlib.h>

leak(void *p) ~ [ p = malloc(1); ]
{
        p = malloc(1);
}
