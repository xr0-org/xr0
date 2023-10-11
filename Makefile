# commands
CC = gcc -g -Wreturn-type -std=gnu11
CFLAGS = -I include -Wall

LEX = lex
YACC = bison -yvd

# dirs
BIN_DIR = bin
BUILD_DIR = build
INCLUDE_DIR = include
SRC_DIR = src
VERIFY_DIR = $(SRC_DIR)/verify
STATE_DIR = $(SRC_DIR)/state
AST_DIR = $(SRC_DIR)/ast
UTIL_DIR = $(SRC_DIR)/util
MATH_DIR = $(SRC_DIR)/math

# executable
XR0V = $(BIN_DIR)/0v

# build artifacts
MAIN_OBJ = $(BUILD_DIR)/main.o
VERIFY_OBJ = $(BUILD_DIR)/verify.o

STATE_OBJ = $(BUILD_DIR)/state.o
STACK_OBJ = $(BUILD_DIR)/stack.o
HEAP_OBJ = $(BUILD_DIR)/heap.o
LOCATION_OBJ = $(BUILD_DIR)/location.o
BLOCK_OBJ = $(BUILD_DIR)/block.o
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
	      $(VERIFY_OBJ) \
	      $(MATH_OBJ)

STATE_OBJECTS = $(VALUE_OBJ) \
		$(LOCATION_OBJ) \
		$(OBJECT_OBJ) \
		$(BLOCK_OBJ) \
		$(HEAP_OBJ) \
		$(STACK_OBJ)

OBJECTS = $(XR0_OBJECTS) $(STATE_OBJECTS)

$(XR0V): $(MAIN_OBJ) $(BIN_DIR)
	@printf 'CC\t$@\n'
	@$(CC) $(CFLAGS) -o $@ $(MAIN_OBJ) $(OBJECTS)

matrix: $(XR0V)
	valgrind $(XR0V) -I libx tests/3-program/matrix.x

matrix-leaks: $(XR0V)
	valgrind --leak-check=full \
		$(XR0V) -I libx tests/3-program/matrix.x

matrix-verbose: $(XR0V) 
	valgrind --num-callers=30 \
		$(XR0V) -I libx tests/3-program/matrix.x

$(MAIN_OBJ): main.c $(OBJECTS)
	@printf 'CC\t$@\n'
	@$(CC) $(CFLAGS) -o $@ -c main.c

$(VERIFY_OBJ): $(VERIFY_DIR)/verify.c $(STATE_OBJ)
	@printf 'CC\t$@\n'
	@$(CC) $(CFLAGS) -o $@ -c $(VERIFY_DIR)/verify.c

$(STATE_OBJ): $(STATE_DIR)/state.c $(STATE_OBJECTS)
	@printf 'CC\t$@\n'
	@$(CC) $(CFLAGS) -o $@ -c $(STATE_DIR)/state.c

$(STACK_OBJ): $(STATE_DIR)/stack.c $(BLOCK_OBJ)
	@printf 'CC\t$@\n'
	@$(CC) $(CFLAGS) -o $@ -c $(STATE_DIR)/stack.c

$(HEAP_OBJ): $(STATE_DIR)/heap.c $(BLOCK_OBJ)
	@printf 'CC\t$@\n'
	@$(CC) $(CFLAGS) -o $@ -c $(STATE_DIR)/heap.c

$(BLOCK_OBJ): $(STATE_DIR)/block.c $(OBJECT_OBJ)
	@printf 'CC\t$@\n'
	@$(CC) $(CFLAGS) -o $@ -c $(STATE_DIR)/block.c

$(OBJECT_OBJ): $(STATE_DIR)/object.c $(VALUE_OBJ)
	@printf 'CC\t$@\n'
	@$(CC) $(CFLAGS) -o $@ -c $(STATE_DIR)/object.c

$(VALUE_OBJ): $(STATE_DIR)/value.c $(LOCATION_OBJ)
	@printf 'CC\t$@\n'
	@$(CC) $(CFLAGS) -o $@ -c $(STATE_DIR)/value.c

$(LOCATION_OBJ): $(STATE_DIR)/location.c $(UTIL_OBJ)
	@printf 'CC\t$@\n'
	@$(CC) $(CFLAGS) -o $@ -c $(STATE_DIR)/location.c

$(UTIL_OBJ): $(UTIL_DIR)/util.c $(BUILD_DIR)
	@printf 'CC\t$@\n'
	@$(CC) $(CFLAGS) -o $@ -c $(UTIL_DIR)/util.c

$(AST_OBJ): $(AST_DIR)/ast.c $(MATH_OBJ)
	@printf 'CC\t$@\n'
	@$(CC) $(CFLAGS) -o $@ -c $(AST_DIR)/ast.c

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
	@$(CC) $(CFLAGS) -o $@ -c $(GRAM_TAB_C)

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
	valgrind $(XR0V) -I libx $(filter-out $@,$(MAKECMDGOALS))

check-verbose: $(RUNTEST) $(TESTFILES) $(XR0V)
	valgrind --num-callers=30 \
		$(XR0V) -v -I libx $(filter-out $@,$(MAKECMDGOALS))

clean:
	@rm -rf $(BUILD_DIR) $(BIN_DIR) $(OBJECTS) $(XR0_OBJECTS)
