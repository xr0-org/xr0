include <stdlib.h>

notleak(void **p) ~ [
        setup: p = .clump(1);
        *p = malloc(1);
]{
        *p = malloc(1);
}
