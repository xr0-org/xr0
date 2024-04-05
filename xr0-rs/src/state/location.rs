#![allow(
    dead_code,
    mutable_transmutes,
    non_camel_case_types,
    non_snake_case,
    non_upper_case_globals,
    unused_assignments,
    unused_mut,
    unused_variables
)]

use crate::{
    static_memory, vconst, AstExpr, Block, Clump, Heap, Object, Stack, State, StrBuilder, Value,
};

extern "C" {
    fn printf(_: *const libc::c_char, _: ...) -> libc::c_int;
    fn malloc(_: libc::c_ulong) -> *mut libc::c_void;
    fn free(_: *mut libc::c_void);
    fn __assert_rtn(
        _: *const libc::c_char,
        _: *const libc::c_char,
        _: libc::c_int,
        _: *const libc::c_char,
    ) -> !;
    fn ast_expr_constant_create(_: libc::c_int) -> *mut AstExpr;
    fn ast_expr_destroy(_: *mut AstExpr);
    fn ast_expr_str(_: *mut AstExpr) -> *mut libc::c_char;
    fn ast_expr_copy(_: *mut AstExpr) -> *mut AstExpr;
    fn ast_expr_equal(e1: *mut AstExpr, e2: *mut AstExpr) -> bool;
    fn error_create(s: *mut libc::c_char) -> *mut error;
    fn strbuilder_build(b: *mut StrBuilder) -> *mut libc::c_char;
    fn strbuilder_printf(b: *mut StrBuilder, fmt: *const libc::c_char, _: ...) -> libc::c_int;
    fn strbuilder_create() -> *mut StrBuilder;
    fn block_str(_: *mut Block) -> *mut libc::c_char;
    fn block_references(_: *mut Block, _: *mut Location, _: *mut State) -> bool;
    fn block_range_aredeallocands(
        _: *mut Block,
        lw: *mut AstExpr,
        up: *mut AstExpr,
        _: *mut State,
    ) -> bool;
    fn block_range_dealloc(
        _: *mut Block,
        lw: *mut AstExpr,
        up: *mut AstExpr,
        _: *mut State,
    ) -> *mut error;
    fn clump_getblock(c: *mut Clump, address: libc::c_int) -> *mut Block;
    fn static_memory_getblock(_: *mut static_memory, address: libc::c_int) -> *mut Block;
    fn heap_getblock(h: *mut Heap, Block: libc::c_int) -> *mut Block;
    fn heap_blockisfreed(h: *mut Heap, Block: libc::c_int) -> bool;
    fn heap_deallocblock(h: *mut Heap, Block: libc::c_int) -> *mut error;
    fn state_get(State: *mut State, loc: *mut Location, constructive: bool) -> object_res;
    fn state_getblock(State: *mut State, loc: *mut Location) -> *mut Block;
    fn object_referencesheap(_: *mut Object, _: *mut State) -> bool;
    fn stack_getframe(s: *mut Stack, frame: libc::c_int) -> *mut Stack;
    fn stack_getblock(_: *mut Stack, address: libc::c_int) -> *mut Block;
    fn state_getheap(_: *mut State) -> *mut Heap;
    fn state_alloc(_: *mut State) -> *mut Value;
    fn state_clump(_: *mut State) -> *mut Value;
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct error {
    pub msg: *mut libc::c_char,
    pub inner: *mut error,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct Location {
    pub type_0: location_type,
    pub u: C2RustUnnamed,
    pub Block: libc::c_int,
    pub offset: *mut AstExpr,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub union C2RustUnnamed {
    pub frame: libc::c_int,
}
pub type location_type = libc::c_uint;
pub const LOCATION_DYNAMIC: location_type = 4;
pub const LOCATION_AUTOMATIC: location_type = 3;
pub const LOCATION_DEREFERENCABLE: location_type = 2;
pub const LOCATION_VCONST: location_type = 1;
pub const LOCATION_STATIC: location_type = 0;
#[derive(Copy, Clone)]
#[repr(C)]
pub struct object_res {
    pub obj: *mut Object,
    pub err: *mut error,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct block_res {
    pub b: *mut Block,
    pub err: *mut error,
}
#[no_mangle]
pub unsafe extern "C" fn location_create_vconst(
    mut Block: libc::c_int,
    mut offset: *mut AstExpr,
) -> *mut Location {
    let mut loc: *mut Location =
        malloc(::core::mem::size_of::<Location>() as libc::c_ulong) as *mut Location;
    if loc.is_null() as libc::c_int as libc::c_long != 0 {
        __assert_rtn(
            (*::core::mem::transmute::<&[u8; 23], &[libc::c_char; 23]>(
                b"location_create_vconst\0",
            ))
            .as_ptr(),
            b"Location.c\0" as *const u8 as *const libc::c_char,
            31 as libc::c_int,
            b"loc\0" as *const u8 as *const libc::c_char,
        );
    } else {
    };
    (*loc).type_0 = LOCATION_VCONST;
    (*loc).Block = Block;
    if offset.is_null() as libc::c_int as libc::c_long != 0 {
        __assert_rtn(
            (*::core::mem::transmute::<&[u8; 23], &[libc::c_char; 23]>(
                b"location_create_vconst\0",
            ))
            .as_ptr(),
            b"Location.c\0" as *const u8 as *const libc::c_char,
            34 as libc::c_int,
            b"offset\0" as *const u8 as *const libc::c_char,
        );
    } else {
    };
    (*loc).offset = offset;
    return loc;
}
#[no_mangle]
pub unsafe extern "C" fn location_create_dereferencable(
    mut Block: libc::c_int,
    mut offset: *mut AstExpr,
) -> *mut Location {
    let mut loc: *mut Location =
        malloc(::core::mem::size_of::<Location>() as libc::c_ulong) as *mut Location;
    if loc.is_null() as libc::c_int as libc::c_long != 0 {
        __assert_rtn(
            (*::core::mem::transmute::<&[u8; 31], &[libc::c_char; 31]>(
                b"location_create_dereferencable\0",
            ))
            .as_ptr(),
            b"Location.c\0" as *const u8 as *const libc::c_char,
            43 as libc::c_int,
            b"loc\0" as *const u8 as *const libc::c_char,
        );
    } else {
    };
    (*loc).type_0 = LOCATION_DEREFERENCABLE;
    (*loc).Block = Block;
    if offset.is_null() as libc::c_int as libc::c_long != 0 {
        __assert_rtn(
            (*::core::mem::transmute::<&[u8; 31], &[libc::c_char; 31]>(
                b"location_create_dereferencable\0",
            ))
            .as_ptr(),
            b"Location.c\0" as *const u8 as *const libc::c_char,
            46 as libc::c_int,
            b"offset\0" as *const u8 as *const libc::c_char,
        );
    } else {
    };
    (*loc).offset = offset;
    return loc;
}
#[no_mangle]
pub unsafe extern "C" fn location_create_static(
    mut Block: libc::c_int,
    mut offset: *mut AstExpr,
) -> *mut Location {
    let mut loc: *mut Location =
        malloc(::core::mem::size_of::<Location>() as libc::c_ulong) as *mut Location;
    if loc.is_null() as libc::c_int as libc::c_long != 0 {
        __assert_rtn(
            (*::core::mem::transmute::<&[u8; 23], &[libc::c_char; 23]>(
                b"location_create_static\0",
            ))
            .as_ptr(),
            b"Location.c\0" as *const u8 as *const libc::c_char,
            55 as libc::c_int,
            b"loc\0" as *const u8 as *const libc::c_char,
        );
    } else {
    };
    (*loc).type_0 = LOCATION_STATIC;
    (*loc).Block = Block;
    if offset.is_null() as libc::c_int as libc::c_long != 0 {
        __assert_rtn(
            (*::core::mem::transmute::<&[u8; 23], &[libc::c_char; 23]>(
                b"location_create_static\0",
            ))
            .as_ptr(),
            b"Location.c\0" as *const u8 as *const libc::c_char,
            58 as libc::c_int,
            b"offset\0" as *const u8 as *const libc::c_char,
        );
    } else {
    };
    (*loc).offset = offset;
    return loc;
}
#[no_mangle]
pub unsafe extern "C" fn location_create_dynamic(
    mut Block: libc::c_int,
    mut offset: *mut AstExpr,
) -> *mut Location {
    let mut loc: *mut Location =
        malloc(::core::mem::size_of::<Location>() as libc::c_ulong) as *mut Location;
    if loc.is_null() as libc::c_int as libc::c_long != 0 {
        __assert_rtn(
            (*::core::mem::transmute::<&[u8; 24], &[libc::c_char; 24]>(
                b"location_create_dynamic\0",
            ))
            .as_ptr(),
            b"Location.c\0" as *const u8 as *const libc::c_char,
            67 as libc::c_int,
            b"loc\0" as *const u8 as *const libc::c_char,
        );
    } else {
    };
    (*loc).type_0 = LOCATION_DYNAMIC;
    (*loc).Block = Block;
    if offset.is_null() as libc::c_int as libc::c_long != 0 {
        __assert_rtn(
            (*::core::mem::transmute::<&[u8; 24], &[libc::c_char; 24]>(
                b"location_create_dynamic\0",
            ))
            .as_ptr(),
            b"Location.c\0" as *const u8 as *const libc::c_char,
            70 as libc::c_int,
            b"offset\0" as *const u8 as *const libc::c_char,
        );
    } else {
    };
    (*loc).offset = offset;
    return loc;
}
#[no_mangle]
pub unsafe extern "C" fn location_create_automatic(
    mut frame: libc::c_int,
    mut Block: libc::c_int,
    mut offset: *mut AstExpr,
) -> *mut Location {
    let mut loc: *mut Location =
        malloc(::core::mem::size_of::<Location>() as libc::c_ulong) as *mut Location;
    if loc.is_null() as libc::c_int as libc::c_long != 0 {
        __assert_rtn(
            (*::core::mem::transmute::<&[u8; 26], &[libc::c_char; 26]>(
                b"location_create_automatic\0",
            ))
            .as_ptr(),
            b"Location.c\0" as *const u8 as *const libc::c_char,
            79 as libc::c_int,
            b"loc\0" as *const u8 as *const libc::c_char,
        );
    } else {
    };
    (*loc).type_0 = LOCATION_AUTOMATIC;
    (*loc).u.frame = frame;
    (*loc).Block = Block;
    if offset.is_null() as libc::c_int as libc::c_long != 0 {
        __assert_rtn(
            (*::core::mem::transmute::<&[u8; 26], &[libc::c_char; 26]>(
                b"location_create_automatic\0",
            ))
            .as_ptr(),
            b"Location.c\0" as *const u8 as *const libc::c_char,
            83 as libc::c_int,
            b"offset\0" as *const u8 as *const libc::c_char,
        );
    } else {
    };
    (*loc).offset = offset;
    return loc;
}
#[no_mangle]
pub unsafe extern "C" fn location_transfigure(
    mut loc: *mut Location,
    mut compare: *mut State,
) -> *mut Value {
    match (*loc).type_0 as libc::c_uint {
        3 | 2 => return state_clump(compare),
        4 => return state_alloc(compare),
        _ => {
            if (0 as libc::c_int == 0) as libc::c_int as libc::c_long != 0 {
                __assert_rtn(
                    (*::core::mem::transmute::<&[u8; 21], &[libc::c_char; 21]>(
                        b"location_transfigure\0",
                    ))
                    .as_ptr(),
                    b"Location.c\0" as *const u8 as *const libc::c_char,
                    98 as libc::c_int,
                    b"false\0" as *const u8 as *const libc::c_char,
                );
            } else {
            };
        }
    }
    panic!("Reached end of non-void function without returning");
}
#[no_mangle]
pub unsafe extern "C" fn location_destroy(mut loc: *mut Location) {
    ast_expr_destroy((*loc).offset);
    free(loc as *mut libc::c_void);
}
#[no_mangle]
pub unsafe extern "C" fn location_str(mut loc: *mut Location) -> *mut libc::c_char {
    let mut b: *mut StrBuilder = strbuilder_create();
    match (*loc).type_0 as libc::c_uint {
        0 => {
            strbuilder_printf(b, b"static:\0" as *const u8 as *const libc::c_char);
        }
        3 => {
            strbuilder_printf(
                b,
                b"Stack[%d]:\0" as *const u8 as *const libc::c_char,
                (*loc).u.frame,
            );
        }
        4 => {
            strbuilder_printf(b, b"Heap:\0" as *const u8 as *const libc::c_char);
        }
        2 => {
            strbuilder_printf(b, b"Clump:\0" as *const u8 as *const libc::c_char);
        }
        _ => {
            if (0 as libc::c_int == 0) as libc::c_int as libc::c_long != 0 {
                __assert_rtn(
                    (*::core::mem::transmute::<&[u8; 13], &[libc::c_char; 13]>(b"location_str\0"))
                        .as_ptr(),
                    b"Location.c\0" as *const u8 as *const libc::c_char,
                    130 as libc::c_int,
                    b"false\0" as *const u8 as *const libc::c_char,
                );
            } else {
            };
        }
    }
    strbuilder_printf(b, b"%d\0" as *const u8 as *const libc::c_char, (*loc).Block);
    if !offsetzero(loc) {
        let mut offset: *mut libc::c_char = ast_expr_str((*loc).offset);
        strbuilder_printf(b, b"+%s\0" as *const u8 as *const libc::c_char, offset);
        free(offset as *mut libc::c_void);
    }
    return strbuilder_build(b);
}
unsafe extern "C" fn offsetzero(mut loc: *mut Location) -> bool {
    let mut zero: *mut AstExpr = ast_expr_constant_create(0 as libc::c_int);
    let mut eq: bool = ast_expr_equal((*loc).offset, zero);
    ast_expr_destroy(zero);
    return eq;
}
#[no_mangle]
pub unsafe extern "C" fn location_type(mut loc: *mut Location) -> location_type {
    return (*loc).type_0;
}
#[no_mangle]
pub unsafe extern "C" fn location_block(mut loc: *mut Location) -> libc::c_int {
    return (*loc).Block;
}
#[no_mangle]
pub unsafe extern "C" fn location_offset(mut loc: *mut Location) -> *mut AstExpr {
    return (*loc).offset;
}
#[no_mangle]
pub unsafe extern "C" fn location_copy(mut loc: *mut Location) -> *mut Location {
    match (*loc).type_0 as libc::c_uint {
        0 => return location_create_static((*loc).Block, ast_expr_copy((*loc).offset)),
        1 => return location_create_vconst((*loc).Block, ast_expr_copy((*loc).offset)),
        2 => {
            return location_create_dereferencable((*loc).Block, ast_expr_copy((*loc).offset));
        }
        3 => {
            return location_create_automatic(
                (*loc).u.frame,
                (*loc).Block,
                ast_expr_copy((*loc).offset),
            );
        }
        4 => return location_create_dynamic((*loc).Block, ast_expr_copy((*loc).offset)),
        _ => {
            if (0 as libc::c_int == 0) as libc::c_int as libc::c_long != 0 {
                __assert_rtn(
                    (*::core::mem::transmute::<&[u8; 14], &[libc::c_char; 14]>(b"location_copy\0"))
                        .as_ptr(),
                    b"Location.c\0" as *const u8 as *const libc::c_char,
                    193 as libc::c_int,
                    b"false\0" as *const u8 as *const libc::c_char,
                );
            } else {
            };
        }
    }
    panic!("Reached end of non-void function without returning");
}
#[no_mangle]
pub unsafe extern "C" fn location_with_offset(
    mut loc: *mut Location,
    mut offset: *mut AstExpr,
) -> *mut Location {
    if !offsetzero(loc) as libc::c_int as libc::c_long != 0 {
        __assert_rtn(
            (*::core::mem::transmute::<&[u8; 21], &[libc::c_char; 21]>(b"location_with_offset\0"))
                .as_ptr(),
            b"Location.c\0" as *const u8 as *const libc::c_char,
            201 as libc::c_int,
            b"offsetzero(loc)\0" as *const u8 as *const libc::c_char,
        );
    } else {
    };
    let mut copy: *mut Location = location_copy(loc);
    (*copy).offset = ast_expr_copy(offset);
    return copy;
}
#[no_mangle]
pub unsafe extern "C" fn location_tostatic(
    mut loc: *mut Location,
    mut sm: *mut static_memory,
) -> bool {
    let mut type_equal: bool =
        (*loc).type_0 as libc::c_uint == LOCATION_STATIC as libc::c_int as libc::c_uint;
    let mut b: *mut Block = static_memory_getblock(sm, (*loc).Block);
    return type_equal as libc::c_int != 0 && !b.is_null();
}
#[no_mangle]
pub unsafe extern "C" fn location_toheap(mut loc: *mut Location, mut h: *mut Heap) -> bool {
    let mut type_equal: bool =
        (*loc).type_0 as libc::c_uint == LOCATION_DYNAMIC as libc::c_int as libc::c_uint;
    let mut b: *mut Block = heap_getblock(h, (*loc).Block);
    return type_equal as libc::c_int != 0 && !b.is_null();
}
#[no_mangle]
pub unsafe extern "C" fn location_tostack(mut loc: *mut Location, mut s: *mut Stack) -> bool {
    let mut type_equal: bool =
        (*loc).type_0 as libc::c_uint == LOCATION_AUTOMATIC as libc::c_int as libc::c_uint;
    let mut b: *mut Block = stack_getblock(s, (*loc).Block);
    return type_equal as libc::c_int != 0 && !b.is_null();
}
#[no_mangle]
pub unsafe extern "C" fn location_toclump(mut loc: *mut Location, mut c: *mut Clump) -> bool {
    let mut type_equal: bool =
        (*loc).type_0 as libc::c_uint == LOCATION_DEREFERENCABLE as libc::c_int as libc::c_uint;
    let mut b: *mut Block = clump_getblock(c, (*loc).Block);
    return type_equal as libc::c_int != 0 && !b.is_null();
}
#[no_mangle]
pub unsafe extern "C" fn location_equal(mut l1: *mut Location, mut l2: *mut Location) -> bool {
    return (*l1).type_0 as libc::c_uint == (*l2).type_0 as libc::c_uint
        && (*l1).Block == (*l2).Block
        && ast_expr_equal((*l1).offset, (*l2).offset) as libc::c_int != 0;
}
#[no_mangle]
pub unsafe extern "C" fn location_references(
    mut l1: *mut Location,
    mut l2: *mut Location,
    mut s: *mut State,
) -> bool {
    if location_equal(l1, l2) {
        return 1 as libc::c_int != 0;
    }
    let mut b: *mut Block = state_getblock(s, l1);
    return !b.is_null() && block_references(b, l2, s) as libc::c_int != 0;
}
#[no_mangle]
pub unsafe extern "C" fn location_isauto(mut loc: *mut Location) -> bool {
    return (*loc).type_0 as libc::c_uint == LOCATION_AUTOMATIC as libc::c_int as libc::c_uint;
}
#[no_mangle]
pub unsafe extern "C" fn location_referencesheap(mut l: *mut Location, mut s: *mut State) -> bool {
    if (*l).type_0 as libc::c_uint == LOCATION_DYNAMIC as libc::c_int as libc::c_uint {
        if heap_blockisfreed(state_getheap(s), (*l).Block) {
            return 0 as libc::c_int != 0;
        }
        return 1 as libc::c_int != 0;
    }
    let mut res: object_res = state_get(s, l, 0 as libc::c_int != 0);
    if !(res.err).is_null() {
        if (0 as libc::c_int == 0) as libc::c_int as libc::c_long != 0 {
            __assert_rtn(
                (*::core::mem::transmute::<&[u8; 24], &[libc::c_char; 24]>(
                    b"location_referencesheap\0",
                ))
                .as_ptr(),
                b"Location.c\0" as *const u8 as *const libc::c_char,
                279 as libc::c_int,
                b"false\0" as *const u8 as *const libc::c_char,
            );
        } else {
        };
    }
    return !(res.obj).is_null() && object_referencesheap(res.obj, s) as libc::c_int != 0;
}
#[no_mangle]
pub unsafe extern "C" fn location_getblock(
    mut loc: *mut Location,
    mut sm: *mut static_memory,
    mut v: *mut vconst,
    mut s: *mut Stack,
    mut h: *mut Heap,
    mut c: *mut Clump,
) -> block_res {
    if s.is_null() as libc::c_int as libc::c_long != 0 {
        __assert_rtn(
            (*::core::mem::transmute::<&[u8; 18], &[libc::c_char; 18]>(b"location_getblock\0"))
                .as_ptr(),
            b"Location.c\0" as *const u8 as *const libc::c_char,
            291 as libc::c_int,
            b"s\0" as *const u8 as *const libc::c_char,
        );
    } else {
    };
    match (*loc).type_0 as libc::c_uint {
        0 => {
            return {
                let mut init = block_res {
                    b: static_memory_getblock(sm, (*loc).Block),
                    err: 0 as *mut error,
                };
                init
            };
        }
        3 => return location_auto_getblock(loc, s),
        4 => {
            return {
                let mut init = block_res {
                    b: heap_getblock(h, (*loc).Block),
                    err: 0 as *mut error,
                };
                init
            };
        }
        2 => {
            return {
                let mut init = block_res {
                    b: clump_getblock(c, (*loc).Block),
                    err: 0 as *mut error,
                };
                init
            };
        }
        _ => {
            if (0 as libc::c_int == 0) as libc::c_int as libc::c_long != 0 {
                __assert_rtn(
                    (*::core::mem::transmute::<&[u8; 18], &[libc::c_char; 18]>(
                        b"location_getblock\0",
                    ))
                    .as_ptr(),
                    b"Location.c\0" as *const u8 as *const libc::c_char,
                    311 as libc::c_int,
                    b"false\0" as *const u8 as *const libc::c_char,
                );
            } else {
            };
        }
    }
    panic!("Reached end of non-void function without returning");
}
unsafe extern "C" fn location_auto_getblock(
    mut loc: *mut Location,
    mut s: *mut Stack,
) -> block_res {
    let mut f: *mut Stack = stack_getframe(s, (*loc).u.frame);
    if f.is_null() {
        return {
            let mut init = block_res {
                b: 0 as *mut Block,
                err: error_create(
                    b"Stack frame doesn't exist\0" as *const u8 as *const libc::c_char
                        as *mut libc::c_char,
                ),
            };
            init
        };
    }
    return {
        let mut init = block_res {
            b: stack_getblock(f, (*loc).Block),
            err: 0 as *mut error,
        };
        init
    };
}
#[no_mangle]
pub unsafe extern "C" fn location_getstackblock(
    mut loc: *mut Location,
    mut s: *mut Stack,
) -> *mut Block {
    if !((*loc).type_0 as libc::c_uint == LOCATION_AUTOMATIC as libc::c_int as libc::c_uint)
        as libc::c_int as libc::c_long
        != 0
    {
        __assert_rtn(
            (*::core::mem::transmute::<&[u8; 23], &[libc::c_char; 23]>(
                b"location_getstackblock\0",
            ))
            .as_ptr(),
            b"Location.c\0" as *const u8 as *const libc::c_char,
            334 as libc::c_int,
            b"loc->type == LOCATION_AUTOMATIC\0" as *const u8 as *const libc::c_char,
        );
    } else {
    };
    return stack_getblock(s, (*loc).Block);
}
#[no_mangle]
pub unsafe extern "C" fn location_dealloc(
    mut loc: *mut Location,
    mut Heap: *mut Heap,
) -> *mut error {
    if (*loc).type_0 as libc::c_uint != LOCATION_DYNAMIC as libc::c_int as libc::c_uint {
        return error_create(
            b"not Heap Location\0" as *const u8 as *const libc::c_char as *mut libc::c_char,
        );
    }
    return heap_deallocblock(Heap, (*loc).Block);
}
#[no_mangle]
pub unsafe extern "C" fn location_range_dealloc(
    mut loc: *mut Location,
    mut lw: *mut AstExpr,
    mut up: *mut AstExpr,
    mut State: *mut State,
) -> *mut error {
    if !offsetzero(loc) as libc::c_int as libc::c_long != 0 {
        __assert_rtn(
            (*::core::mem::transmute::<&[u8; 23], &[libc::c_char; 23]>(
                b"location_range_dealloc\0",
            ))
            .as_ptr(),
            b"Location.c\0" as *const u8 as *const libc::c_char,
            352 as libc::c_int,
            b"offsetzero(loc)\0" as *const u8 as *const libc::c_char,
        );
    } else {
    };
    let mut b: *mut Block = state_getblock(State, loc);
    if b.is_null() {
        return error_create(
            b"cannot get Block\0" as *const u8 as *const libc::c_char as *mut libc::c_char,
        );
    }
    if !block_range_aredeallocands(b, lw, up, State) {
        printf(
            b"Block: %s\n\0" as *const u8 as *const libc::c_char,
            block_str(b),
        );
        printf(
            b"lw: %s, up: %s\n\0" as *const u8 as *const libc::c_char,
            ast_expr_str(lw),
            ast_expr_str(up),
        );
        if (0 as libc::c_int == 0) as libc::c_int as libc::c_long != 0 {
            __assert_rtn(
                (*::core::mem::transmute::<&[u8; 23], &[libc::c_char; 23]>(
                    b"location_range_dealloc\0",
                ))
                .as_ptr(),
                b"Location.c\0" as *const u8 as *const libc::c_char,
                362 as libc::c_int,
                b"false\0" as *const u8 as *const libc::c_char,
            );
        } else {
        };
        return error_create(
            b"some values not allocated\0" as *const u8 as *const libc::c_char as *mut libc::c_char,
        );
    }
    return block_range_dealloc(b, lw, up, State);
}
