#ifndef _AST_VARIABLE_MAP_H_
#define _AST_VARIABLE_MAP_H_

#undef TYPE
#undef TYPED

#define TYPE struct ast_variable *
#define TYPED(THING) ast_variable_ ## THING

#include "generic_map.h"

#endif
