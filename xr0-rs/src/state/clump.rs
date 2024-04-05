#![allow(
    dead_code,
    mutable_transmutes,
    non_camel_case_types,
    non_snake_case,
    non_upper_case_globals,
    unused_assignments,
    unused_mut
)]

use crate::{block_arr, Block, StrBuilder};

extern "C" {
    fn malloc(_: libc::c_ulong) -> *mut libc::c_void;
    fn free(_: *mut libc::c_void);
    fn __assert_rtn(
        _: *const libc::c_char,
        _: *const libc::c_char,
        _: libc::c_int,
        _: *const libc::c_char,
    ) -> !;
    fn strbuilder_create() -> *mut StrBuilder;
    fn strbuilder_printf(b: *mut StrBuilder, fmt: *const libc::c_char, _: ...) -> libc::c_int;
    fn strbuilder_build(b: *mut StrBuilder) -> *mut libc::c_char;
    fn block_create() -> *mut Block;
    fn block_str(_: *mut Block) -> *mut libc::c_char;
    fn block_arr_create() -> *mut block_arr;
    fn block_arr_destroy(_: *mut block_arr);
    fn block_arr_copy(_: *mut block_arr) -> *mut block_arr;
    fn block_arr_blocks(_: *mut block_arr) -> *mut *mut Block;
    fn block_arr_nblocks(_: *mut block_arr) -> libc::c_int;
    fn block_arr_append(_: *mut block_arr, _: *mut Block) -> libc::c_int;
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct Clump {
    pub blocks: *mut block_arr,
}
#[no_mangle]
pub unsafe extern "C" fn clump_create() -> *mut Clump {
    let mut c: *mut Clump = malloc(::core::mem::size_of::<Clump>() as libc::c_ulong) as *mut Clump;
    if c.is_null() as libc::c_int as libc::c_long != 0 {
        __assert_rtn(
            (*::core::mem::transmute::<&[u8; 13], &[libc::c_char; 13]>(b"clump_create\0")).as_ptr(),
            b"Clump.c\0" as *const u8 as *const libc::c_char,
            18 as libc::c_int,
            b"c\0" as *const u8 as *const libc::c_char,
        );
    } else {
    };
    (*c).blocks = block_arr_create();
    return c;
}
#[no_mangle]
pub unsafe extern "C" fn clump_destroy(mut c: *mut Clump) {
    block_arr_destroy((*c).blocks);
}
#[no_mangle]
pub unsafe extern "C" fn clump_str(
    mut c: *mut Clump,
    mut indent: *mut libc::c_char,
) -> *mut libc::c_char {
    let mut b: *mut StrBuilder = strbuilder_create();
    let mut n: libc::c_int = block_arr_nblocks((*c).blocks);
    let mut arr: *mut *mut Block = block_arr_blocks((*c).blocks);
    let mut i: libc::c_int = 0 as libc::c_int;
    while i < n {
        let mut Block: *mut libc::c_char = block_str(*arr.offset(i as isize));
        strbuilder_printf(
            b,
            b"%s%d: %s\n\0" as *const u8 as *const libc::c_char,
            indent,
            i,
            Block,
        );
        free(Block as *mut libc::c_void);
        i += 1;
    }
    return strbuilder_build(b);
}
#[no_mangle]
pub unsafe extern "C" fn clump_copy(mut c: *mut Clump) -> *mut Clump {
    let mut copy: *mut Clump =
        malloc(::core::mem::size_of::<Clump>() as libc::c_ulong) as *mut Clump;
    (*copy).blocks = block_arr_copy((*c).blocks);
    return copy;
}
#[no_mangle]
pub unsafe extern "C" fn clump_newblock(mut c: *mut Clump) -> libc::c_int {
    let mut address: libc::c_int = block_arr_append((*c).blocks, block_create());
    let mut n: libc::c_int = block_arr_nblocks((*c).blocks);
    if !(n > 0 as libc::c_int) as libc::c_int as libc::c_long != 0 {
        __assert_rtn(
            (*::core::mem::transmute::<&[u8; 15], &[libc::c_char; 15]>(b"clump_newblock\0"))
                .as_ptr(),
            b"Clump.c\0" as *const u8 as *const libc::c_char,
            57 as libc::c_int,
            b"n > 0\0" as *const u8 as *const libc::c_char,
        );
    } else {
    };
    return address;
}
#[no_mangle]
pub unsafe extern "C" fn clump_getblock(mut c: *mut Clump, mut address: libc::c_int) -> *mut Block {
    if address >= block_arr_nblocks((*c).blocks) {
        return 0 as *mut Block;
    }
    return *(block_arr_blocks((*c).blocks)).offset(address as isize);
}
