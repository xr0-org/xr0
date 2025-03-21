#include <stdlib.h>

struct list {
	int x;
	struct list *next;
};

struct list *
f() ~ [
	struct list *list;

	list = malloc(sizeof(struct list));
	list->x = 0;
	list->next = malloc(sizeof(struct list));
	list->next->x = [0?10];
	return list;
]{
	struct list *list;

	list = malloc(sizeof(struct list));
	list->x = 0;
	list->next = malloc(sizeof(struct list));
	list->next->x = 1;
	return list;
}
