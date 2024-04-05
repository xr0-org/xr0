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

use crate::{block_arr, AstExpr, Block, Location, State, StrBuilder, Value};

extern "C" {
    fn malloc(_: libc::c_ulong) -> *mut libc::c_void;
    fn free(_: *mut libc::c_void);
    fn realloc(_: *mut libc::c_void, _: libc::c_ulong) -> *mut libc::c_void;
    fn __assert_rtn(
        _: *const libc::c_char,
        _: *const libc::c_char,
        _: libc::c_int,
        _: *const libc::c_char,
    ) -> !;
    fn ast_expr_constant_create(_: libc::c_int) -> *mut AstExpr;
    fn ast_expr_matheval(e: *mut AstExpr) -> bool;
    fn strbuilder_printf(b: *mut StrBuilder, fmt: *const libc::c_char, _: ...) -> libc::c_int;
    fn strbuilder_create() -> *mut StrBuilder;
    fn map_set(_: *mut map, key: *const libc::c_char, Value: *const libc::c_void);
    fn map_destroy(_: *mut map);
    fn map_create() -> *mut map;
    fn dynamic_str(_: *const libc::c_char) -> *mut libc::c_char;
    fn strbuilder_build(b: *mut StrBuilder) -> *mut libc::c_char;
    fn error_create(s: *mut libc::c_char) -> *mut error;
    fn map_get(_: *mut map, key: *const libc::c_char) -> *mut libc::c_void;
    fn block_create() -> *mut Block;
    fn block_str(_: *mut Block) -> *mut libc::c_char;
    fn block_undeclare(_: *mut Block, _: *mut State);
    fn block_arr_create() -> *mut block_arr;
    fn block_arr_destroy(_: *mut block_arr);
    fn block_arr_copy(_: *mut block_arr) -> *mut block_arr;
    fn block_arr_blocks(_: *mut block_arr) -> *mut *mut Block;
    fn block_arr_nblocks(_: *mut block_arr) -> libc::c_int;
    fn block_arr_append(_: *mut block_arr, _: *mut Block) -> libc::c_int;
    fn state_references(s: *mut State, loc: *mut Location) -> bool;
    fn location_create_dynamic(Block: libc::c_int, offset: *mut AstExpr) -> *mut Location;
    fn location_destroy(_: *mut Location);
    fn value_copy(_: *mut Value) -> *mut Value;
    fn value_destroy(_: *mut Value);
    fn value_str(_: *mut Value) -> *mut libc::c_char;
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct map {
    pub entry: *mut entry,
    pub n: libc::c_int,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct entry {
    pub key: *mut libc::c_char,
    pub Value: *const libc::c_void,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct error {
    pub msg: *mut libc::c_char,
    pub inner: *mut error,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct Heap {
    pub blocks: *mut block_arr,
    pub freed: *mut bool,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct vconst {
    pub varmap: *mut map,
    pub comment: *mut map,
    pub persist: *mut map,
}
#[no_mangle]
pub unsafe extern "C" fn heap_create() -> *mut Heap {
    let mut h: *mut Heap = malloc(::core::mem::size_of::<Heap>() as libc::c_ulong) as *mut Heap;
    if h.is_null() as libc::c_int as libc::c_long != 0 {
        __assert_rtn(
            (*::core::mem::transmute::<&[u8; 12], &[libc::c_char; 12]>(b"heap_create\0")).as_ptr(),
            b"Heap.c\0" as *const u8 as *const libc::c_char,
            25 as libc::c_int,
            b"h\0" as *const u8 as *const libc::c_char,
        );
    } else {
    };
    (*h).blocks = block_arr_create();
    (*h).freed = 0 as *mut bool;
    return h;
}
#[no_mangle]
pub unsafe extern "C" fn heap_destroy(mut h: *mut Heap) {
    block_arr_destroy((*h).blocks);
    free((*h).freed as *mut libc::c_void);
    free(h as *mut libc::c_void);
}
#[no_mangle]
pub unsafe extern "C" fn heap_copy(mut h: *mut Heap) -> *mut Heap {
    let mut copy: *mut Heap = malloc(::core::mem::size_of::<Heap>() as libc::c_ulong) as *mut Heap;
    (*copy).blocks = block_arr_copy((*h).blocks);
    let mut n: libc::c_int = block_arr_nblocks((*h).blocks);
    let mut freed_copy: *mut bool =
        malloc((::core::mem::size_of::<bool>() as libc::c_ulong).wrapping_mul(n as libc::c_ulong))
            as *mut bool;
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < n {
        *freed_copy.offset(i as isize) = *((*h).freed).offset(i as isize);
        i += 1;
    }
    (*copy).freed = freed_copy;
    return copy;
}
#[no_mangle]
pub unsafe extern "C" fn heap_str(
    mut h: *mut Heap,
    mut indent: *mut libc::c_char,
) -> *mut libc::c_char {
    let mut b: *mut StrBuilder = strbuilder_create();
    let mut arr: *mut *mut Block = block_arr_blocks((*h).blocks);
    let mut n: libc::c_int = block_arr_nblocks((*h).blocks);
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < n {
        if !*((*h).freed).offset(i as isize) {
            let mut Block: *mut libc::c_char = block_str(*arr.offset(i as isize));
            strbuilder_printf(
                b,
                b"%s%d: %s%s\0" as *const u8 as *const libc::c_char,
                indent,
                i,
                Block,
                if printdelim(h, i) as libc::c_int != 0 {
                    b"\n\0" as *const u8 as *const libc::c_char
                } else {
                    b"\0" as *const u8 as *const libc::c_char
                },
            );
            free(Block as *mut libc::c_void);
        }
        i += 1;
    }
    return strbuilder_build(b);
}
unsafe extern "C" fn printdelim(mut h: *mut Heap, mut start: libc::c_int) -> bool {
    let mut n: libc::c_int = block_arr_nblocks((*h).blocks);
    let mut i: libc::c_int = start + 1 as libc::c_int;
    while i < n {
        if !*((*h).freed).offset(i as isize) {
            return 1 as libc::c_int != 0;
        }
        i += 1;
    }
    return 0 as libc::c_int != 0;
}
#[no_mangle]
pub unsafe extern "C" fn heap_blocks(mut h: *mut Heap) -> *mut block_arr {
    if h.is_null() as libc::c_int as libc::c_long != 0 {
        __assert_rtn(
            (*::core::mem::transmute::<&[u8; 12], &[libc::c_char; 12]>(b"heap_blocks\0")).as_ptr(),
            b"Heap.c\0" as *const u8 as *const libc::c_char,
            91 as libc::c_int,
            b"h\0" as *const u8 as *const libc::c_char,
        );
    } else {
    };
    return (*h).blocks;
}
#[no_mangle]
pub unsafe extern "C" fn heap_newblock(mut h: *mut Heap) -> *mut Location {
    let mut address: libc::c_int = block_arr_append((*h).blocks, block_create());
    let mut n: libc::c_int = block_arr_nblocks((*h).blocks);
    if !(n > 0 as libc::c_int) as libc::c_int as libc::c_long != 0 {
        __assert_rtn(
            (*::core::mem::transmute::<&[u8; 14], &[libc::c_char; 14]>(b"heap_newblock\0"))
                .as_ptr(),
            b"Heap.c\0" as *const u8 as *const libc::c_char,
            101 as libc::c_int,
            b"n > 0\0" as *const u8 as *const libc::c_char,
        );
    } else {
    };
    (*h).freed = realloc(
        (*h).freed as *mut libc::c_void,
        (::core::mem::size_of::<bool>() as libc::c_ulong).wrapping_mul(n as libc::c_ulong),
    ) as *mut bool;
    *((*h).freed).offset(address as isize) = 0 as libc::c_int != 0;
    return location_create_dynamic(address, ast_expr_constant_create(0 as libc::c_int));
}
#[no_mangle]
pub unsafe extern "C" fn heap_getblock(mut h: *mut Heap, mut address: libc::c_int) -> *mut Block {
    if address >= block_arr_nblocks((*h).blocks) {
        return 0 as *mut Block;
    }
    if *((*h).freed).offset(address as isize) {
        return 0 as *mut Block;
    }
    return *(block_arr_blocks((*h).blocks)).offset(address as isize);
}
#[no_mangle]
pub unsafe extern "C" fn heap_deallocblock(
    mut h: *mut Heap,
    mut address: libc::c_int,
) -> *mut error {
    if !(address < block_arr_nblocks((*h).blocks)) as libc::c_int as libc::c_long != 0 {
        __assert_rtn(
            (*::core::mem::transmute::<&[u8; 18], &[libc::c_char; 18]>(b"heap_deallocblock\0"))
                .as_ptr(),
            b"Heap.c\0" as *const u8 as *const libc::c_char,
            125 as libc::c_int,
            b"address < block_arr_nblocks(h->blocks)\0" as *const u8 as *const libc::c_char,
        );
    } else {
    };
    if *((*h).freed).offset(address as isize) {
        return error_create(
            b"double free\0" as *const u8 as *const libc::c_char as *mut libc::c_char,
        );
    }
    *((*h).freed).offset(address as isize) = 1 as libc::c_int != 0;
    return 0 as *mut error;
}
#[no_mangle]
pub unsafe extern "C" fn heap_blockisfreed(mut h: *mut Heap, mut address: libc::c_int) -> bool {
    return *((*h).freed).offset(address as isize);
}
#[no_mangle]
pub unsafe extern "C" fn heap_undeclare(mut h: *mut Heap, mut s: *mut State) {
    let mut n: libc::c_int = block_arr_nblocks((*h).blocks);
    let mut b: *mut *mut Block = block_arr_blocks((*h).blocks);
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < n {
        if !*((*h).freed).offset(i as isize) {
            block_undeclare(*b.offset(i as isize), s);
        }
        i += 1;
    }
}
#[no_mangle]
pub unsafe extern "C" fn heap_referenced(mut h: *mut Heap, mut s: *mut State) -> bool {
    let mut n: libc::c_int = block_arr_nblocks((*h).blocks);
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < n {
        if !*((*h).freed).offset(i as isize) && !block_referenced(s, i) {
            return 0 as libc::c_int != 0;
        }
        i += 1;
    }
    return 1 as libc::c_int != 0;
}
unsafe extern "C" fn block_referenced(mut s: *mut State, mut addr: libc::c_int) -> bool {
    let mut loc: *mut Location =
        location_create_dynamic(addr, ast_expr_constant_create(0 as libc::c_int));
    let mut referenced: bool = state_references(s, loc);
    location_destroy(loc);
    return referenced;
}
#[no_mangle]
pub unsafe extern "C" fn vconst_create() -> *mut vconst {
    let mut v: *mut vconst =
        malloc(::core::mem::size_of::<vconst>() as libc::c_ulong) as *mut vconst;
    (*v).varmap = map_create();
    (*v).comment = map_create();
    (*v).persist = map_create();
    return v;
}
#[no_mangle]
pub unsafe extern "C" fn vconst_destroy(mut v: *mut vconst) {
    let mut m: *mut map = (*v).varmap;
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < (*m).n {
        value_destroy((*((*m).entry).offset(i as isize)).Value as *mut Value);
        i += 1;
    }
    map_destroy(m);
    map_destroy((*v).comment);
    map_destroy((*v).persist);
    free(v as *mut libc::c_void);
}
#[no_mangle]
pub unsafe extern "C" fn vconst_copy(mut old: *mut vconst) -> *mut vconst {
    let mut new: *mut vconst = vconst_create();
    let mut m: *mut map = (*old).varmap;
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < (*m).n {
        let mut e: entry = *((*m).entry).offset(i as isize);
        map_set(
            (*new).varmap,
            dynamic_str(e.key),
            value_copy(e.Value as *mut Value) as *const libc::c_void,
        );
        i += 1;
    }
    m = (*old).comment;
    let mut i_0: libc::c_int = 0 as libc::c_int;
    while i_0 < (*m).n {
        let mut e_0: entry = *((*m).entry).offset(i_0 as isize);
        map_set(
            (*new).comment,
            dynamic_str(e_0.key),
            dynamic_str(e_0.Value as *const libc::c_char) as *const libc::c_void,
        );
        i_0 += 1;
    }
    m = (*old).persist;
    let mut i_1: libc::c_int = 0 as libc::c_int;
    while i_1 < (*m).n {
        let mut e_1: entry = *((*m).entry).offset(i_1 as isize);
        map_set((*new).persist, dynamic_str(e_1.key), e_1.Value);
        i_1 += 1;
    }
    return new;
}
#[no_mangle]
pub unsafe extern "C" fn vconst_declare(
    mut v: *mut vconst,
    mut val: *mut Value,
    mut comment: *mut libc::c_char,
    mut persist: bool,
) -> *mut libc::c_char {
    let mut m: *mut map = (*v).varmap;
    let mut s: *mut libc::c_char = vconst_id(m, (*v).persist, persist);
    map_set(m, dynamic_str(s), val as *const libc::c_void);
    if !comment.is_null() {
        map_set((*v).comment, dynamic_str(s), comment as *const libc::c_void);
    }
    map_set(
        (*v).persist,
        dynamic_str(s),
        persist as usize as *mut libc::c_void,
    );
    return s;
}
unsafe extern "C" fn vconst_id(
    mut varmap: *mut map,
    mut persistmap: *mut map,
    mut persist: bool,
) -> *mut libc::c_char {
    let mut npersist: libc::c_int = count_true(persistmap);
    let mut b: *mut StrBuilder = strbuilder_create();
    if persist {
        strbuilder_printf(b, b"$%d\0" as *const u8 as *const libc::c_char, npersist);
    } else {
        strbuilder_printf(
            b,
            b"#%d\0" as *const u8 as *const libc::c_char,
            (*varmap).n - npersist,
        );
    }
    return strbuilder_build(b);
}
unsafe extern "C" fn count_true(mut m: *mut map) -> libc::c_int {
    let mut n: libc::c_int = 0 as libc::c_int;
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < (*m).n {
        if !((*((*m).entry).offset(i as isize)).Value).is_null() {
            n += 1;
        }
        i += 1;
    }
    return n;
}
#[no_mangle]
pub unsafe extern "C" fn vconst_get(mut v: *mut vconst, mut id: *mut libc::c_char) -> *mut Value {
    return map_get((*v).varmap, id) as *mut Value;
}
#[no_mangle]
pub unsafe extern "C" fn vconst_undeclare(mut v: *mut vconst) {
    let mut varmap: *mut map = map_create();
    let mut comment: *mut map = map_create();
    let mut persist: *mut map = map_create();
    let mut m: *mut map = (*v).varmap;
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < (*m).n {
        let mut key: *mut libc::c_char = (*((*m).entry).offset(i as isize)).key;
        if !(map_get((*v).persist, key)).is_null() {
            map_set(
                varmap,
                dynamic_str(key),
                value_copy(map_get((*v).varmap, key) as *mut Value) as *const libc::c_void,
            );
            let mut c: *mut libc::c_char = map_get((*v).comment, key) as *mut libc::c_char;
            if !c.is_null() {
                map_set(
                    comment,
                    dynamic_str(key),
                    dynamic_str(c) as *const libc::c_void,
                );
            }
            map_set(
                persist,
                dynamic_str(key),
                1 as libc::c_int as *mut libc::c_void,
            );
        }
        i += 1;
    }
    (*v).varmap = varmap;
    (*v).comment = comment;
    (*v).persist = persist;
}
#[no_mangle]
pub unsafe extern "C" fn vconst_str(
    mut v: *mut vconst,
    mut indent: *mut libc::c_char,
) -> *mut libc::c_char {
    let mut b: *mut StrBuilder = strbuilder_create();
    let mut m: *mut map = (*v).varmap;
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < (*m).n {
        let mut e: entry = *((*m).entry).offset(i as isize);
        let mut Value: *mut libc::c_char = value_str(e.Value as *mut Value);
        strbuilder_printf(
            b,
            b"%s%s: %s\0" as *const u8 as *const libc::c_char,
            indent,
            e.key,
            Value,
        );
        let mut comment: *mut libc::c_char = map_get((*v).comment, e.key) as *mut libc::c_char;
        if !comment.is_null() {
            strbuilder_printf(b, b"\t(%s)\0" as *const u8 as *const libc::c_char, comment);
        }
        strbuilder_printf(b, b"\n\0" as *const u8 as *const libc::c_char);
        free(Value as *mut libc::c_void);
        i += 1;
    }
    return strbuilder_build(b);
}
#[no_mangle]
pub unsafe extern "C" fn vconst_eval(mut v: *mut vconst, mut e: *mut AstExpr) -> bool {
    return ast_expr_matheval(e);
}
