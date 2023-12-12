#ifndef _AST_FUNCTION_MAP_H_
#define _AST_FUNCTION_MAP_H_

#undef TYPE
#undef TYPED

#define TYPE struct ast_function *
#define TYPED(THING) ast_function_ ## THING

#include "generic_map.h"

#endif
