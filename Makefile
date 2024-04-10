# commands
CC = gcc -g -Wreturn-type -std=gnu11
CFLAGS = -I include -Wall
VALGRIND = valgrind --fullpath-after=`pwd`/src/

LEX = lex
YACC = bison -yvd

# dirs
BIN_DIR = bin
BUILD_DIR = build
RUST_DEPS_DIR = xr0-deps/target/debug
INCLUDE_DIR = include
SRC_DIR = src
UTIL_DIR = $(SRC_DIR)/util

SRC_0V_DIR = $(SRC_DIR)

EXT_DIR = $(SRC_0V_DIR)/ext
PROPS_DIR = $(SRC_0V_DIR)/props
STATE_DIR = $(SRC_0V_DIR)/state
OBJECT_DIR = $(SRC_0V_DIR)/object
VALUE_DIR = $(SRC_0V_DIR)/value
AST_DIR = $(SRC_0V_DIR)/ast
MATH_DIR = $(SRC_0V_DIR)/math

# executable
XR0V = $(BIN_DIR)/0v

XR0V_RUST = xr0-rs/target/debug/xr0

# includes
INCLUDES = \
include/props.h \
include/gram_util.h \
include/value.h \
include/ast.h \
include/lex.h \
include/storage.h \
include/util.h \
include/math.h \
include/object.h \
include/ext.h \
include/state.h \
include/verify.h \
build/gram.tab.h \
src/state/clump.h \
src/state/intern.h \
src/state/static.h \
src/state/stack.h \
src/state/heap.h \
src/state/block.h \
src/state/location.h \
src/ast/stmt/stmt.h \
src/ast/intern.h \
src/ast/topological.h \
src/ast/expr/expr.h \
src/ast/function/function.h \
src/ast/literals.h \
src/ast/type/type.h

C_SOURCES = \
build/gram.tab.c \
build/lex.yy.c \
src/util/util.c \
src/props/props.c \
src/ext/ext.c \
src/value/value.c \
src/state/static.c \
src/state/block.c \
src/state/heap.c \
src/state/stack.c \
src/state/state.c \
src/state/location.c \
src/state/clump.c \
src/state/test.c \
src/math/math.c \
src/math/test.c \
src/main.c \
src/object/object.c \
src/ast/stmt/stmt.c \
src/ast/stmt/verify.c \
src/ast/externdecl.c \
src/ast/ast.c \
src/ast/block.c \
src/ast/expr/verify.c \
src/ast/expr/expr.c \
src/ast/function/function.c \
src/ast/function/arr.c \
src/ast/type/type.c \
src/ast/literals.c \
src/ast/topological.c \
src/ast/variable.c

RUST_SOURCES = \
$(SRC_0V_DIR)/main.rs \
$(AST_DIR)/ast.rs \
$(EXT_DIR)/ext.rs \
$(MATH_DIR)/math.rs \
$(OBJECT_DIR)/object.rs \
$(PROPS_DIR)/props.rs \
$(STATE_DIR)/block.rs \
$(STATE_DIR)/clump.rs \
$(STATE_DIR)/heap.rs \
$(STATE_DIR)/location.rs \
$(STATE_DIR)/stack.rs \
$(STATE_DIR)/state.rs \
$(STATE_DIR)/static.rs \
$(UTIL_DIR)/util.rs \
$(VALUE_DIR)/value.rs \
$(BUILD_DIR)/lex.yy.rs \
$(NULL)

# build artifacts
MAIN_0V_OBJ = $(BUILD_DIR)/0v.o

STATE_OBJ = $(BUILD_DIR)/state.o
STATIC_OBJ = $(BUILD_DIR)/static.o
STACK_OBJ = $(BUILD_DIR)/stack.o
HEAP_OBJ = $(BUILD_DIR)/heap.o
CLUMP_OBJ = $(BUILD_DIR)/clump.o
LOCATION_OBJ = $(BUILD_DIR)/location.o
BLOCK_OBJ = $(BUILD_DIR)/block.o
EXT_OBJ = $(BUILD_DIR)/ext.o
PROPS_OBJ = $(BUILD_DIR)/props.o
OBJECT_OBJ = $(BUILD_DIR)/object.o
VALUE_OBJ = $(BUILD_DIR)/value.o
MATH_OBJ = $(BUILD_DIR)/math.o

AST_OBJ = $(BUILD_DIR)/ast.o
LEX_OBJ = $(BUILD_DIR)/lex.o
UTIL_OBJ = $(BUILD_DIR)/util.o
GRAM_OBJ = $(BUILD_DIR)/gram.o

GRAM_TAB_C = $(BUILD_DIR)/gram.tab.c
GRAM_TAB_H = $(BUILD_DIR)/gram.tab.h
LEX_YY_C = $(BUILD_DIR)/lex.yy.c

XR0_OBJECTS = $(AST_OBJ) \
	      $(LEX_OBJ) \
	      $(GRAM_OBJ) \
	      $(STATE_OBJ) \
	      $(UTIL_OBJ) \
	      $(MATH_OBJ)

STATE_OBJECTS = $(VALUE_OBJ) \
		$(LOCATION_OBJ) \
		$(OBJECT_OBJ) \
		$(BLOCK_OBJ) \
		$(CLUMP_OBJ) \
		$(HEAP_OBJ) \
		$(STACK_OBJ) \
		$(STATIC_OBJ) \
		$(EXT_OBJ) \
		$(PROPS_OBJ)

C2RUST = ../c2rust/target/release/c2rust

OBJECTS = $(XR0_OBJECTS) $(STATE_OBJECTS)

RUSTCFLAGS = --edition=2021 --extern core --extern libc=$(LIBC_RLIB)

$(XR0V): $(SRC_0V_DIR)/main.rs $(LIBC_RLIB) $(OBJECTS)
	@printf 'RUSTC\t$@\n'
	@rustc +nightly --crate-name xr0 $(RUSTCFLAGS) -o $@ $< $(foreach obj,$(OBJECTS),-Clink-arg=$(obj))

$(C2RUST):
	(cd ../c2rust && cargo build --release)

lex: $(XR0V)
	$(VALGRIND) $(XR0V) -I libx tests/99-program/100-lex/parse.x

PARSER = $(BUILD_DIR)/lex-gen

$(RUST_SOURCES): $(C2RUST) compile_commands.json $(C_SOURCES) $(INCLUDES)
	rm -f $(RUST_SOURCES)
	$(C2RUST) transpile --fail-on-error compile_commands.json
	if ! [ -f compile_commands.json ]; then git checkout compile_commands.json; fi

lex-gen:
	@$(XR0C) tests/5-program/100-lex/parse.x > build/parse.c
	@c89 -g -o $(PARSER) $(BUILD_DIR)/parse.c
	@$(PARSER) > $(BUILD_DIR)/gen_firstchar
	@echo '%' > $(BUILD_DIR)/percent
	@diff $(BUILD_DIR)/gen_firstchar $(BUILD_DIR)/percent

lex-leaks: $(XR0V)
	$(VALGRIND) --leak-check=full \
		$(XR0V) -I libx tests/3-program/100-lex/parse.x

lex-verbose: $(XR0V) 
	$(VALGRIND) --num-callers=30 \
		$(XR0V) -I libx tests/3-program/100-lex/parse.x

matrix: $(XR0V)
	$(VALGRIND) $(XR0V) -I libx tests/3-program/000-matrix.x

matrix-leaks: $(XR0V)
	$(VALGRIND) --leak-check=full \
		$(XR0V) -I libx tests/3-program/000-matrix.x

matrix-verbose: $(XR0V) 
	$(VALGRIND) --num-callers=30 \
		$(XR0V) -I libx tests/3-program/000-matrix.x

LIBC_RLIB = xr0-deps/target/debug/deps/liblibc-7b7b9cd53da27782.rlib

$(LIBC_RLIB):
	(cd xr0-deps && cargo +nightly build)

$(STATE_OBJ): $(STATE_DIR)/state.rs $(LIBC_RLIB) $(BIN_DIR)
	@printf 'RUSTC\t$@\n'
	@rustc +nightly --crate-name xr0_state $(RUSTCFLAGS) --crate-type staticlib -o $@ $<

$(EXT_OBJ): $(EXT_DIR)/ext.rs $(LIBC_RLIB)
	@printf 'RUSTC\t$@\n'
	@rustc +nightly --crate-name xr0_ext $(RUSTCFLAGS) --crate-type staticlib -o $@ $<

$(PROPS_OBJ): $(PROPS_DIR)/props.rs $(LIBC_RLIB)
	@printf 'RUSTC\t$@\n'
	@rustc +nightly --crate-name xr0_props $(RUSTCFLAGS) --crate-type staticlib -o $@ $<

$(STACK_OBJ): $(STATE_DIR)/stack.rs $(LIBC_RLIB)
	@printf 'RUSTC\t$@\n'
	@rustc +nightly --crate-name xr0_stack $(RUSTCFLAGS) --crate-type staticlib -o $@ $<

$(HEAP_OBJ): $(STATE_DIR)/heap.rs $(LIBC_RLIB)
	@printf 'RUSTC\t$@\n'
	@rustc +nightly --crate-name xr0_heap $(RUSTCFLAGS) --crate-type staticlib -o $@ $<

$(CLUMP_OBJ): $(STATE_DIR)/clump.rs $(LIBC_RLIB)
	@printf 'RUSTC\t$@\n'
	@rustc +nightly --crate-name xr0_clump $(RUSTCFLAGS) --crate-type staticlib -o $@ $<

$(STATIC_OBJ): $(STATE_DIR)/static.rs $(LIBC_RLIB)
	@printf 'RUSTC\t$@\n'
	@rustc +nightly --crate-name xr0_static $(RUSTCFLAGS) --crate-type staticlib -o $@ $<

$(BLOCK_OBJ): $(STATE_DIR)/block.rs $(LIBC_RLIB)
	@printf 'RUSTC\t$@\n'
	@rustc +nightly --crate-name xr0_block $(RUSTCFLAGS) --crate-type staticlib -o $@ $<

$(OBJECT_OBJ): $(OBJECT_DIR)/object.rs $(LIBC_RLIB)
	@printf 'RUSTC\t$@\n'
	@rustc +nightly --crate-name xr0_object $(RUSTCFLAGS) --crate-type staticlib -o $@ $<

$(VALUE_OBJ): $(VALUE_DIR)/value.rs $(LIBC_RLIB)
	@printf 'RUSTC\t$@\n'
	@rustc +nightly --crate-name xr0_value $(RUSTCFLAGS) --crate-type staticlib -o $@ $<

$(LOCATION_OBJ): $(STATE_DIR)/location.rs $(LIBC_RLIB)
	@printf 'RUSTC\t$@\n'
	@rustc +nightly --crate-name xr0_location $(RUSTCFLAGS) --crate-type staticlib -o $@ $<

$(UTIL_OBJ): $(UTIL_DIR)/util.rs $(LIBC_RLIB)
	@printf 'RUSTC\t$@\n'
	@rustc +nightly --crate-name xr0_util $(RUSTCFLAGS) --crate-type staticlib -o $@ $<

$(AST_OBJ): $(AST_DIR)/ast.rs $(LIBC_RLIB)
	@printf 'RUSTC\t$@\n'
	@rustc +nightly --crate-name xr0_ast $(RUSTCFLAGS) --crate-type staticlib -o $@ $<

$(MATH_OBJ): $(MATH_DIR)/math.rs $(LIBC_RLIB)
	@printf 'RUSTC\t$@\n'
	@rustc +nightly --crate-name xr0_math $(RUSTCFLAGS) --crate-type staticlib -o $@ $<

$(LEX_OBJ): $(BUILD_DIR)/lex.yy.rs $(LIBC_RLIB)
	@printf 'RUSTC\t$@\n'
	@rustc +nightly --crate-name xr0_lex $(RUSTCFLAGS) --crate-type staticlib -o $@ $<

$(LEX_YY_C): $(INCLUDE_DIR)/lex.h $(AST_DIR)/lex.l
	@printf 'LEX\t$@\n'
	@$(LEX) -o $(BUILD_DIR)/lex.yy.c $(AST_DIR)/lex.l

$(GRAM_OBJ): $(GRAM_TAB_C) $(INCLUDES)
	@printf 'CC\t$@\n'
	@$(CC) $(CFLAGS) -I $(AST_DIR) -o $@ -c $(GRAM_TAB_C)

$(GRAM_TAB_C) $(GRAM_TAB_H): $(AST_DIR)/gram.y
	@printf 'YACC\t$@\n'
	@mkdir -p $(BUILD_DIR)
	@$(YACC) -o $(BUILD_DIR)/gram.tab.c -d $(AST_DIR)/gram.y

$(BIN_DIR):
	@mkdir -p $(BIN_DIR)

# tests
TESTDIR = tests
RUNTEST = $(TESTDIR)/run
TESTFILES = $(shell find $(TESTDIR) -name '*.0')

test: $(RUNTEST) $(TESTFILES) $(XR0V) 
	XR0=$(XR0V) ./tests/run

.PHONY: $(XR0V_RUST)
$(XR0V_RUST):
	(cd xr0-rs && cargo build)

.PHONY: test-rust
test-rust: $(XR0V_RUST)
	XR0=$(XR0V_RUST) ./tests/run

.PHONY: check
check: $(XR0V)
	$(VALGRIND) $(XR0V) -I libx $(filter-out $@,$(MAKECMDGOALS))

.PHONY: check-verbose
check-verbose: $(XR0V)
	$(VALGRIND) --num-callers=30 \
		$(XR0V) -v -I libx $(filter-out $@,$(MAKECMDGOALS))

.PHONY: clean
clean:
	@rm -rf $(BUILD_DIR) $(BIN_DIR) $(OBJECTS) $(XR0_OBJECTS)
