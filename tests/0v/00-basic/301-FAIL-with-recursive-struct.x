/* 
 * thanks to jorendorff
 * https://github.com/xr0-org/xr0/issues/46
 */

#include <stdlib.h>

struct node {
	struct node *next;
};

#ifdef XR0

struct node *
f() ~ [
	struct node one;

	one.next = &one;
        return &one;
];

#endif

struct node *
f()
{
	struct node one;

	one.next = &one;
	malloc(1);
        return &one;
}
