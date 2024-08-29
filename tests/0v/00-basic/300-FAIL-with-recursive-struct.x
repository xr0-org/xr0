/*
 * thanks to jorendorff
 * https://github.com/xr0-org/xr0/issues/46
 */

#include <stdlib.h>

struct node {
        struct node *next;
};

struct node *
f() ~ [
	struct node *one;

        one = malloc(sizeof(struct node));
        one->next = one;
        return one;
]{
	struct node *one;

        one = malloc(sizeof(struct node));
        one->next = one;
        malloc(1);
        return one;
}
