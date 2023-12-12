#ifndef _AST_TYPE_MAP_H_
#define _AST_TYPE_MAP_H_

#undef TYPE
#undef TYPED

#define TYPE struct ast_type *
#define TYPED(THING) ast_type_ ## THING

#include "generic_map.h"

#endif
