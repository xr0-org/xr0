#ifndef _OBJECT_MAP_H_
#define _OBJECT_MAP_H_

#undef TYPE
#undef TYPED

#define TYPE struct object *
#define TYPED(THING) object_ ## THING

#include "generic_map.h"

#endif
