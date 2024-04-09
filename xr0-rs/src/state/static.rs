#![allow(
    dead_code,
    mutable_transmutes,
    non_camel_case_types,
    non_snake_case,
    non_upper_case_globals,
    unused_assignments,
    unused_mut
)]

use libc::{free, malloc};

use crate::c_util::__assert_rtn;
use crate::state::block::{
    block_arr_append, block_arr_blocks, block_arr_copy, block_arr_create, block_arr_destroy,
    block_arr_nblocks, block_create, block_str,
};
use crate::state::location::location_copy;
use crate::util::{
    dynamic_str, entry, map, map_create, map_get, map_set, strbuilder_build, strbuilder_create,
    strbuilder_printf,
};
use crate::{block_arr, Block as block, Location as location, StrBuilder as strbuilder};

#[derive(Copy, Clone)]
#[repr(C)]
pub struct static_memory {
    pub blocks: *mut block_arr,
    pub pool: *mut map,
}
#[no_mangle]
pub unsafe fn static_memory_create() -> *mut static_memory {
    let mut sm: *mut static_memory =
        malloc(::core::mem::size_of::<static_memory>()) as *mut static_memory;
    if sm.is_null() as libc::c_int as libc::c_long != 0 {
        __assert_rtn(
            (*::core::mem::transmute::<&[u8; 21], &[libc::c_char; 21]>(b"static_memory_create\0"))
                .as_ptr(),
            b"static.c\0" as *const u8 as *const libc::c_char,
            20 as libc::c_int,
            b"sm\0" as *const u8 as *const libc::c_char,
        );
    } else {
    };
    (*sm).blocks = block_arr_create();
    (*sm).pool = map_create();
    return sm;
}
#[no_mangle]
pub unsafe fn static_memory_destroy(mut sm: *mut static_memory) {
    block_arr_destroy((*sm).blocks);
}
#[no_mangle]
pub unsafe fn static_memory_str(
    mut sm: *mut static_memory,
    mut indent: *mut libc::c_char,
) -> *mut libc::c_char {
    let mut b: *mut strbuilder = strbuilder_create();
    let mut n: libc::c_int = block_arr_nblocks((*sm).blocks);
    let mut arr: *mut *mut block = block_arr_blocks((*sm).blocks);
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < n {
        let mut block: *mut libc::c_char = block_str(*arr.offset(i as isize));
        strbuilder_printf(
            b,
            b"%s%d: %s\n\0" as *const u8 as *const libc::c_char,
            indent,
            i,
            block,
        );
        free(block as *mut libc::c_void);
        i += 1;
    }
    return strbuilder_build(b);
}
#[no_mangle]
pub unsafe fn static_memory_copy(mut sm: *mut static_memory) -> *mut static_memory {
    let mut copy: *mut static_memory =
        malloc(::core::mem::size_of::<static_memory>()) as *mut static_memory;
    (*copy).blocks = block_arr_copy((*sm).blocks);
    (*copy).pool = pool_copy((*sm).pool);
    return copy;
}
unsafe fn pool_copy(mut p: *mut map) -> *mut map {
    let mut pcopy: *mut map = map_create();
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < (*p).n {
        let mut e: entry = *((*p).entry).offset(i as isize);
        map_set(
            pcopy,
            dynamic_str(e.key),
            location_copy(e.value as *mut location) as *const libc::c_void,
        );
        i += 1;
    }
    return pcopy;
}
#[no_mangle]
pub unsafe fn static_memory_newblock(mut sm: *mut static_memory) -> libc::c_int {
    let mut address: libc::c_int = block_arr_append((*sm).blocks, block_create());
    let mut n: libc::c_int = block_arr_nblocks((*sm).blocks);
    if !(n > 0 as libc::c_int) as libc::c_int as libc::c_long != 0 {
        __assert_rtn(
            (*::core::mem::transmute::<&[u8; 23], &[libc::c_char; 23]>(
                b"static_memory_newblock\0",
            ))
            .as_ptr(),
            b"static.c\0" as *const u8 as *const libc::c_char,
            80 as libc::c_int,
            b"n > 0\0" as *const u8 as *const libc::c_char,
        );
    } else {
    };
    return address;
}
#[no_mangle]
pub unsafe fn static_memory_getblock(
    mut sm: *mut static_memory,
    mut address: libc::c_int,
) -> *mut block {
    if address >= block_arr_nblocks((*sm).blocks) {
        return 0 as *mut block;
    }
    return *(block_arr_blocks((*sm).blocks)).offset(address as isize);
}
#[no_mangle]
pub unsafe fn static_memory_stringpool(
    mut sm: *mut static_memory,
    mut lit: *mut libc::c_char,
    mut loc: *mut location,
) {
    map_set(
        (*sm).pool,
        dynamic_str(lit),
        location_copy(loc) as *const libc::c_void,
    );
}
#[no_mangle]
pub unsafe fn static_memory_checkpool(
    mut sm: *mut static_memory,
    mut lit: *mut libc::c_char,
) -> *mut location {
    return map_get((*sm).pool, lit) as *mut location;
}
