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
PARSE_DIR = $(SRC_DIR)/parse
LEX_DIR = $(SRC_DIR)/lex
AST_DIR = $(SRC_DIR)/ast
UTIL_DIR = $(SRC_DIR)/util

# executable
XR0V = $(BIN_DIR)/0v

# build artifacts
MAIN_OBJ = $(BUILD_DIR)/main.o
VERIFY_OBJ = $(BUILD_DIR)/verify.o
STATE_OBJ = $(BUILD_DIR)/state.o
AST_OBJ = $(BUILD_DIR)/ast.o
LEX_OBJ = $(BUILD_DIR)/lex.o
UTIL_OBJ = $(BUILD_DIR)/util.o
GRAM_OBJ = $(BUILD_DIR)/gram.o

GRAM_TAB_C = $(BUILD_DIR)/gram.tab.c
GRAM_TAB_H = $(BUILD_DIR)/gram.tab.h
LEX_YY_C = $(BUILD_DIR)/lex.yy.c

OBJECTS = $(AST_OBJ) $(LEX_OBJ) $(GRAM_OBJ) $(STATE_OBJ) $(UTIL_OBJ) $(VERIFY_OBJ)

# tests
TESTDIR = tests
RUNTEST = $(TESTDIR)/run
TESTFILES = $(shell find $(TESTDIR) -name '*.0')

$(XR0V): $(MAIN_OBJ) $(BIN_DIR)
	@printf 'CC\t$@\n'
	@$(CC) $(CFLAGS) -o $@ $(MAIN_OBJ) $(OBJECTS)

$(BIN_DIR):
	@mkdir -p $(BIN_DIR)

$(MAIN_OBJ): main.c $(OBJECTS)
	@printf 'CC\t$@\n'
	@$(CC) $(CFLAGS) -o $@ -c main.c

$(VERIFY_OBJ): $(VERIFY_DIR)/verify.c $(STATE_OBJ)
	@printf 'CC\t$@\n'
	@$(CC) $(CFLAGS) -o $@ -c $(VERIFY_DIR)/verify.c

$(STATE_OBJ): $(STATE_DIR)/state.c $(AST_OBJ) $(UTIL_OBJ)
	@printf 'CC\t$@\n'
	@$(CC) $(CFLAGS) -o $@ -c $(STATE_DIR)/state.c

$(UTIL_OBJ): $(UTIL_DIR)/util.c $(AST_OBJ)
	@printf 'CC\t$@\n'
	@$(CC) $(CFLAGS) -o $@ -c $(UTIL_DIR)/util.c

$(AST_OBJ): $(AST_DIR)/ast.c $(BUILD_DIR)
	@printf 'CC\t$@\n'
	@$(CC) $(CFLAGS) -o $@ -c $(AST_DIR)/ast.c

$(LEX_OBJ): $(LEX_YY_C) $(GRAM_OBJ)
	@printf 'CC\t$@\n'
	@$(CC) $(CFLAGS) -o $@ -c $(LEX_YY_C)

$(LEX_YY_C): $(INCLUDE_DIR)/lex.h $(LEX_DIR)/lex.l
	@printf 'LEX\t$@\n'
	@$(LEX) -o $(BUILD_DIR)/lex.yy.c $(LEX_DIR)/lex.l

$(GRAM_OBJ): $(GRAM_TAB_C) $(GRAM_TAB_H)
	@printf 'CC\t$@\n'
	@$(CC) $(CFLAGS) -o $@ -c $(GRAM_TAB_C)

$(GRAM_TAB_C) $(GRAM_TAB_H): $(PARSE_DIR)/gram.y $(BUILD_DIR)
	@printf 'YACC\t$@\n'
	@$(YACC) -o $(BUILD_DIR)/gram.tab.c -d $(PARSE_DIR)/gram.y

$(BUILD_DIR):
	@mkdir -p $(BUILD_DIR)

test: $(RUNTEST) $(TESTFILES) $(XR0V) 
	@./tests/run

check: $(RUNTEST) $(TESTFILES) $(XR0V)
	valgrind $(XR0V) -I libx $(filter-out $@,$(MAKECMDGOALS))

check-verbose: $(RUNTEST) $(TESTFILES) $(XR0V)
	valgrind --num-callers=30 $(XR0V) -v -I libx $(filter-out $@,$(MAKECMDGOALS))

clean:
	@rm -rf $(BUILD_DIR) $(BIN_DIR) $(OBJECTS)
