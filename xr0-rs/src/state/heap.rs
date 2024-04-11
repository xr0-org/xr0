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

use libc::{free, malloc, realloc};

use crate::ast::{ast_expr_constant_create, ast_expr_matheval};
use crate::c_util::__assert_rtn;
use crate::state::block::{
    block_arr_append, block_arr_blocks, block_arr_copy, block_arr_create, block_arr_destroy,
    block_arr_nblocks, block_create, block_str, block_undeclare,
};
use crate::state::location::{location_create_dynamic, location_destroy};
use crate::state::state::state_references;
use crate::util::{
    dynamic_str, error, error_create, map, strbuilder_build, strbuilder_create, strbuilder_printf,
};
use crate::value::{value_copy, value_destroy, value_str};
use crate::{
    block_arr, AstExpr as ast_expr, Block as block, Location as location, State as state,
    StrBuilder as strbuilder, Value as value,
};

#[derive(Copy, Clone)]
#[repr(C)]
pub struct heap {
    pub blocks: *mut block_arr,
    pub freed: *mut bool,
}
pub struct vconst {
    pub varmap: Box<map>,
    pub comment: Box<map>,
    pub persist: Box<map>,
}

pub unsafe fn heap_create() -> *mut heap {
    let mut h: *mut heap = malloc(::core::mem::size_of::<heap>()) as *mut heap;
    if h.is_null() as libc::c_int as libc::c_long != 0 {
        __assert_rtn(
            (*::core::mem::transmute::<&[u8; 12], &[libc::c_char; 12]>(b"heap_create\0")).as_ptr(),
            b"heap.c\0" as *const u8 as *const libc::c_char,
            25 as libc::c_int,
            b"h\0" as *const u8 as *const libc::c_char,
        );
    } else {
    };
    (*h).blocks = block_arr_create();
    (*h).freed = 0 as *mut bool;
    return h;
}

pub unsafe fn heap_destroy(mut h: *mut heap) {
    block_arr_destroy((*h).blocks);
    free((*h).freed as *mut libc::c_void);
    free(h as *mut libc::c_void);
}

pub unsafe fn heap_copy(mut h: *mut heap) -> *mut heap {
    let mut copy: *mut heap = malloc(::core::mem::size_of::<heap>()) as *mut heap;
    (*copy).blocks = block_arr_copy((*h).blocks);
    let mut n: libc::c_int = block_arr_nblocks((*h).blocks);
    let mut freed_copy: *mut bool =
        malloc((::core::mem::size_of::<bool>()).wrapping_mul(n as usize)) as *mut bool;
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < n {
        *freed_copy.offset(i as isize) = *((*h).freed).offset(i as isize);
        i += 1;
    }
    (*copy).freed = freed_copy;
    return copy;
}

pub unsafe fn heap_str(mut h: *mut heap, mut indent: *mut libc::c_char) -> *mut libc::c_char {
    let mut b: *mut strbuilder = strbuilder_create();
    let mut arr: *mut *mut block = block_arr_blocks((*h).blocks);
    let mut n: libc::c_int = block_arr_nblocks((*h).blocks);
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < n {
        if !*((*h).freed).offset(i as isize) {
            let mut block: *mut libc::c_char = block_str(*arr.offset(i as isize));
            strbuilder_printf(
                b,
                b"%s%d: %s%s\0" as *const u8 as *const libc::c_char,
                indent,
                i,
                block,
                if printdelim(h, i) as libc::c_int != 0 {
                    b"\n\0" as *const u8 as *const libc::c_char
                } else {
                    b"\0" as *const u8 as *const libc::c_char
                },
            );
            free(block as *mut libc::c_void);
        }
        i += 1;
    }
    return strbuilder_build(b);
}
unsafe fn printdelim(mut h: *mut heap, mut start: libc::c_int) -> bool {
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

pub unsafe fn heap_blocks(mut h: *mut heap) -> *mut block_arr {
    if h.is_null() as libc::c_int as libc::c_long != 0 {
        __assert_rtn(
            (*::core::mem::transmute::<&[u8; 12], &[libc::c_char; 12]>(b"heap_blocks\0")).as_ptr(),
            b"heap.c\0" as *const u8 as *const libc::c_char,
            91 as libc::c_int,
            b"h\0" as *const u8 as *const libc::c_char,
        );
    } else {
    };
    return (*h).blocks;
}

pub unsafe fn heap_newblock(mut h: *mut heap) -> *mut location {
    let mut address: libc::c_int = block_arr_append((*h).blocks, block_create());
    let mut n: libc::c_int = block_arr_nblocks((*h).blocks);
    if !(n > 0 as libc::c_int) as libc::c_int as libc::c_long != 0 {
        __assert_rtn(
            (*::core::mem::transmute::<&[u8; 14], &[libc::c_char; 14]>(b"heap_newblock\0"))
                .as_ptr(),
            b"heap.c\0" as *const u8 as *const libc::c_char,
            101 as libc::c_int,
            b"n > 0\0" as *const u8 as *const libc::c_char,
        );
    } else {
    };
    (*h).freed = realloc(
        (*h).freed as *mut libc::c_void,
        (::core::mem::size_of::<bool>()).wrapping_mul(n as usize),
    ) as *mut bool;
    *((*h).freed).offset(address as isize) = 0 as libc::c_int != 0;
    return location_create_dynamic(address, ast_expr_constant_create(0 as libc::c_int));
}

pub unsafe fn heap_getblock(mut h: *mut heap, mut address: libc::c_int) -> *mut block {
    if address >= block_arr_nblocks((*h).blocks) {
        return 0 as *mut block;
    }
    if *((*h).freed).offset(address as isize) {
        return 0 as *mut block;
    }
    return *(block_arr_blocks((*h).blocks)).offset(address as isize);
}

pub unsafe fn heap_deallocblock(mut h: *mut heap, mut address: libc::c_int) -> *mut error {
    if !(address < block_arr_nblocks((*h).blocks)) as libc::c_int as libc::c_long != 0 {
        __assert_rtn(
            (*::core::mem::transmute::<&[u8; 18], &[libc::c_char; 18]>(b"heap_deallocblock\0"))
                .as_ptr(),
            b"heap.c\0" as *const u8 as *const libc::c_char,
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

pub unsafe fn heap_blockisfreed(mut h: *mut heap, mut address: libc::c_int) -> bool {
    return *((*h).freed).offset(address as isize);
}

pub unsafe fn heap_undeclare(mut h: *mut heap, mut s: *mut state) {
    let mut n: libc::c_int = block_arr_nblocks((*h).blocks);
    let mut b: *mut *mut block = block_arr_blocks((*h).blocks);
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < n {
        if !*((*h).freed).offset(i as isize) {
            block_undeclare(*b.offset(i as isize), s);
        }
        i += 1;
    }
}

pub unsafe fn heap_referenced(mut h: *mut heap, mut s: *mut state) -> bool {
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
unsafe fn block_referenced(mut s: *mut state, mut addr: libc::c_int) -> bool {
    let mut loc: *mut location =
        location_create_dynamic(addr, ast_expr_constant_create(0 as libc::c_int));
    let mut referenced: bool = state_references(s, loc);
    location_destroy(loc);
    return referenced;
}

pub unsafe fn vconst_create() -> *mut vconst {
    let mut v: *mut vconst = malloc(::core::mem::size_of::<vconst>()) as *mut vconst;
    std::ptr::write(
        v,
        vconst {
            varmap: map::new(),
            comment: map::new(),
            persist: map::new(),
        },
    );
    return v;
}

pub unsafe fn vconst_destroy(mut v: *mut vconst) {
    let vconst {
        varmap: m,
        comment,
        persist,
    } = std::ptr::read(v);
    for v in m.values() {
        value_destroy(v as *mut value);
    }
    m.destroy();
    comment.destroy();
    persist.destroy();
    free(v as *mut libc::c_void);
}

pub unsafe fn vconst_copy(mut old: *mut vconst) -> *mut vconst {
    let mut new: *mut vconst = vconst_create();
    let mut m = &(*old).varmap;
    for (k, v) in m.pairs() {
        (*new).varmap.set(
            dynamic_str(k),
            value_copy(v as *mut value) as *const libc::c_void,
        );
    }
    m = &(*old).comment;
    for (k, v) in m.pairs() {
        (*new).comment.set(
            dynamic_str(k),
            dynamic_str(v as *const libc::c_char) as *const libc::c_void,
        );
    }
    m = &(*old).persist;
    for (k, v) in m.pairs() {
        (*new).persist.set(dynamic_str(k), v);
    }
    return new;
}

pub unsafe fn vconst_declare(
    mut v: *mut vconst,
    mut val: *mut value,
    mut comment: *mut libc::c_char,
    mut persist: bool,
) -> *mut libc::c_char {
    let mut m = &mut (*v).varmap;
    let mut s: *mut libc::c_char = vconst_id(m, &(*v).persist, persist);
    m.set(dynamic_str(s), val as *const libc::c_void);
    if !comment.is_null() {
        (*v).comment
            .set(dynamic_str(s), comment as *const libc::c_void);
    }
    (*v).persist
        .set(dynamic_str(s), persist as usize as *mut libc::c_void);
    return s;
}
unsafe fn vconst_id(
    mut varmap: &map,
    mut persistmap: &map,
    mut persist: bool,
) -> *mut libc::c_char {
    let mut npersist: libc::c_int = count_true(persistmap);
    let mut b: *mut strbuilder = strbuilder_create();
    if persist {
        strbuilder_printf(b, b"$%d\0" as *const u8 as *const libc::c_char, npersist);
    } else {
        strbuilder_printf(
            b,
            b"#%d\0" as *const u8 as *const libc::c_char,
            varmap.len() - npersist,
        );
    }
    return strbuilder_build(b);
}
unsafe fn count_true(mut m: &map) -> libc::c_int {
    let mut n: libc::c_int = 0 as libc::c_int;
    for v in m.values() {
        if !v.is_null() {
            n += 1;
        }
    }
    return n;
}

pub unsafe fn vconst_get(mut v: *mut vconst, mut id: *mut libc::c_char) -> *mut value {
    return (*v).varmap.get(id) as *mut value;
}

pub unsafe fn vconst_undeclare(mut v: *mut vconst) {
    let mut varmap = map::new();
    let mut comment = map::new();
    let mut persist = map::new();
    let mut m = &(*v).varmap;
    for key in m.keys() {
        if !((*v).persist.get(key)).is_null() {
            varmap.set(
                dynamic_str(key),
                value_copy((*v).varmap.get(key) as *mut value) as *const libc::c_void,
            );
            let mut c: *mut libc::c_char = (*v).comment.get(key) as *mut libc::c_char;
            if !c.is_null() {
                comment.set(dynamic_str(key), dynamic_str(c) as *const libc::c_void);
            }
            persist.set(dynamic_str(key), 1 as libc::c_int as *mut libc::c_void);
        }
    }
    std::ptr::write(
        v,
        vconst {
            varmap,
            comment,
            persist,
        },
    );
}

pub unsafe fn vconst_str(mut v: *mut vconst, mut indent: *mut libc::c_char) -> *mut libc::c_char {
    let mut b: *mut strbuilder = strbuilder_create();
    let mut m = &(*v).varmap;
    for (k, val) in m.pairs() {
        let mut value: *mut libc::c_char = value_str(val as *mut value);
        strbuilder_printf(
            b,
            b"%s%s: %s\0" as *const u8 as *const libc::c_char,
            indent,
            k,
            value,
        );
        let mut comment: *mut libc::c_char = (*v).comment.get(k) as *mut libc::c_char;
        if !comment.is_null() {
            strbuilder_printf(b, b"\t(%s)\0" as *const u8 as *const libc::c_char, comment);
        }
        strbuilder_printf(b, b"\n\0" as *const u8 as *const libc::c_char);
        free(value as *mut libc::c_void);
    }
    return strbuilder_build(b);
}

pub unsafe fn vconst_eval(mut v: *mut vconst, mut e: *mut ast_expr) -> bool {
    return ast_expr_matheval(e);
}
