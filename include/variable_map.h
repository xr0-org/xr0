#ifndef _VARIABLE_MAP_H_
#define _VARIABLE_MAP_H_

#undef TYPE
#undef TYPED

#define TYPE struct variable *
#define TYPED(THING) variable_ ## THING

#include "generic_map.h"

#endif
