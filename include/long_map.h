#ifndef _LONG_MAP_H_
#define _LONG_MAP_H_

#undef TYPE
#undef TYPED

#define TYPE long
#define TYPED(THING) long_ ## THING

#include "generic_map.h"

#endif
