#include <stdlib.h>

#ifdef XR0

leak(void *p) ~ [ p = malloc(1); ];

#endif

leak(void *p)
{
        p = malloc(1);
}
