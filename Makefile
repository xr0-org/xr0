# commands
CC = gcc -g -Wreturn-type -std=gnu11
CFLAGS = -I include -Wall
VALGRIND = valgrind --fullpath-after=`pwd`/src/

LEX = lex
YACC = bison -yvd

# dirs
BIN_DIR = bin
BUILD_DIR = build
INCLUDE_DIR = include
SRC_DIR = src
UTIL_DIR = $(SRC_DIR)/util

SRC_0V_DIR = $(SRC_DIR)/0v
SRC_0C_DIR = $(SRC_DIR)/0c

EXT_DIR = $(SRC_0V_DIR)/ext
PROPS_DIR = $(SRC_0V_DIR)/props
STATE_DIR = $(SRC_0V_DIR)/state
OBJECT_DIR = $(SRC_0V_DIR)/object
VALUE_DIR = $(SRC_0V_DIR)/value
AST_DIR = $(SRC_0V_DIR)/ast
MATH_DIR = $(SRC_0V_DIR)/math

# executable
XR0V = $(BIN_DIR)/0v
XR0C = $(BIN_DIR)/0c

# build artifacts
MAIN_0V_OBJ = $(BUILD_DIR)/0v.o
MAIN_0C_OBJ = $(BUILD_DIR)/0c.o

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

OBJECTS = $(XR0_OBJECTS) $(STATE_OBJECTS)

$(XR0V): $(MAIN_0V_OBJ) $(BIN_DIR)
	@printf 'CC\t$@\n'
	@$(CC) $(CFLAGS) -o $@ $(MAIN_0V_OBJ) $(OBJECTS)

lex: $(XR0V)
	$(VALGRIND) $(XR0V) -I libx tests/99-program/100-lex/parse.x

PARSER = $(BUILD_DIR)/lex-gen

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

$(MAIN_0V_OBJ): $(SRC_0V_DIR)/main.c $(OBJECTS)
	@printf 'CC\t$@\n'
	@$(CC) $(CFLAGS) -I $(BUILD_DIR) -o $@ -c $(SRC_0V_DIR)/main.c

$(STATE_OBJ): $(STATE_DIR)/state.c $(STATE_OBJECTS)
	@printf 'CC\t$@\n'
	@$(CC) $(CFLAGS) -o $@ -c $(STATE_DIR)/state.c

$(EXT_OBJ): $(EXT_DIR)/ext.c
	@printf 'CC\t$@\n'
	@$(CC) $(CFLAGS) -o $@ -c $(EXT_DIR)/ext.c

$(PROPS_OBJ): $(PROPS_DIR)/props.c
	@printf 'CC\t$@\n'
	@$(CC) $(CFLAGS) -o $@ -c $(PROPS_DIR)/props.c

$(STACK_OBJ): $(STATE_DIR)/stack.c $(BLOCK_OBJ)
	@printf 'CC\t$@\n'
	@$(CC) $(CFLAGS) -o $@ -c $(STATE_DIR)/stack.c

$(HEAP_OBJ): $(STATE_DIR)/heap.c $(BLOCK_OBJ)
	@printf 'CC\t$@\n'
	@$(CC) $(CFLAGS) -o $@ -c $(STATE_DIR)/heap.c

$(CLUMP_OBJ): $(STATE_DIR)/clump.c $(BLOCK_OBJ)
	@printf 'CC\t$@\n'
	@$(CC) $(CFLAGS) -o $@ -c $(STATE_DIR)/clump.c

$(STATIC_OBJ): $(STATE_DIR)/static.c $(BLOCK_OBJ)
	@printf 'CC\t$@\n'
	@$(CC) $(CFLAGS) -o $@ -c $(STATE_DIR)/static.c

$(BLOCK_OBJ): $(STATE_DIR)/block.c $(OBJECT_OBJ)
	@printf 'CC\t$@\n'
	@$(CC) $(CFLAGS) -o $@ -c $(STATE_DIR)/block.c

$(OBJECT_OBJ): $(OBJECT_DIR)/object.c $(VALUE_OBJ)
	@printf 'CC\t$@\n'
	@$(CC) $(CFLAGS) -o $@ -c $(OBJECT_DIR)/object.c

$(VALUE_OBJ): $(VALUE_DIR)/value.c $(LOCATION_OBJ)
	@printf 'CC\t$@\n'
	@$(CC) $(CFLAGS) -o $@ -c $(VALUE_DIR)/value.c

$(LOCATION_OBJ): $(STATE_DIR)/location.c $(UTIL_OBJ)
	@printf 'CC\t$@\n'
	@$(CC) $(CFLAGS) -o $@ -c $(STATE_DIR)/location.c

$(UTIL_OBJ): $(UTIL_DIR)/util.c $(BUILD_DIR)
	@printf 'CC\t$@\n'
	@$(CC) $(CFLAGS) -o $@ -c $(UTIL_DIR)/util.c

$(AST_OBJ): $(AST_DIR)/ast.c \
	$(AST_DIR)/topological.h \
	$(AST_DIR)/expr/expr.h \
	$(AST_DIR)/type/type.h \
	$(AST_DIR)/literals.h \
	$(AST_DIR)/function/function.h \
	$(MATH_OBJ)
	@printf 'CC\t$@\n'
	@$(CC) $(CFLAGS) -I $(AST_DIR) -o $@ -c $(AST_DIR)/ast.c

$(MATH_OBJ): $(MATH_DIR)/math.c $(BUILD_DIR)
	@printf 'CC\t$@\n'
	@$(CC) $(CFLAGS) -o $@ -c $(MATH_DIR)/math.c

$(LEX_OBJ): $(LEX_YY_C) $(BUILD_DIR)
	@printf 'CC\t$@\n'
	@$(CC) $(CFLAGS) -o $@ -c $(LEX_YY_C)

$(LEX_YY_C): $(INCLUDE_DIR)/lex.h $(AST_DIR)/lex.l $(GRAM_OBJ)
	@printf 'LEX\t$@\n'
	@$(LEX) -o $(BUILD_DIR)/lex.yy.c $(AST_DIR)/lex.l

$(GRAM_OBJ): $(GRAM_TAB_C) $(GRAM_TAB_H)
	@printf 'CC\t$@\n'
	@$(CC) $(CFLAGS) -I $(AST_DIR) -o $@ -c $(GRAM_TAB_C)

$(GRAM_TAB_C) $(GRAM_TAB_H): $(AST_DIR)/gram.y $(BUILD_DIR)
	@printf 'YACC\t$@\n'
	@$(YACC) -o $(BUILD_DIR)/gram.tab.c -d $(AST_DIR)/gram.y

$(BIN_DIR):
	@mkdir -p $(BIN_DIR)

$(BUILD_DIR):
	@mkdir -p $(BUILD_DIR)

# tests
TESTDIR = tests
RUNTEST = $(TESTDIR)/run
TESTFILES = $(shell find $(TESTDIR) -name '*.0')

test: $(RUNTEST) $(TESTFILES) $(XR0V) 
	@./tests/run

check: $(RUNTEST) $(TESTFILES) $(XR0V)
	$(VALGRIND) $(XR0V) -I libx $(filter-out $@,$(MAKECMDGOALS))

check-verbose: $(RUNTEST) $(TESTFILES) $(XR0V)
	$(VALGRIND) --num-callers=30 \
		$(XR0V) -v -I libx $(filter-out $@,$(MAKECMDGOALS))

clean:
	@rm -rf $(BUILD_DIR) $(BIN_DIR) $(OBJECTS) $(XR0_OBJECTS)
