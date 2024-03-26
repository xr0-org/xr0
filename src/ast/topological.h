#ifndef XR0_TOPOLOGICAL_H
#define XR0_TOPOLOGICAL_H

struct externals;

struct string_arr *
topological_order(char *fname, struct externals *);

#endif
