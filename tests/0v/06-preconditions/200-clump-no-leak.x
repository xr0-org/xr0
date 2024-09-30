#include <stdlib.h>

#ifdef XR0

void
notleak(void **p) ~ [
        setup: p = .clump(1);
        *p = malloc(1);
];

#endif

void
notleak(void **p)
{
        *p = malloc(1);
}
