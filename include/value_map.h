#ifndef _VALUE_MAP_H_
#define _VALUE_MAP_H_

#undef TYPE
#undef TYPED

#define TYPE struct value *
#define TYPED(THING) value_ ## THING

#include "generic_map.h"

#endif
