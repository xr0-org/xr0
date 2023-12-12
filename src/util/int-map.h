#ifndef _INT_MAP_H_
#define _INT_MAP_H_

#undef TYPE
#undef TYPED

#define TYPE int
#define TYPED(THING) int_ ## THING

#include "generic-map.h"

#endif
