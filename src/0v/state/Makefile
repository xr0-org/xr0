# dirs
BIN_DIR = bin
BUILD_DIR = build
XR0_INCLUDE_DIR = ../../include
SRC_DIR = ..
AST_DIR = $(SRC_DIR)/ast
PARSE_DIR = $(SRC_DIR)/parse
LEX_DIR = $(SRC_DIR)/lex
UTIL_DIR = $(SRC_DIR)/util

# executable
TEST = $(BIN_DIR)/test

# commands
CC = gcc
CFLAGS = -I $(XR0_INCLUDE_DIR) -Wall -g -Wreturn-type -std=gnu11

# build artifacts
MAIN_OBJ = $(BUILD_DIR)/test.o
AST_OBJ = $(BUILD_DIR)/ast.o
LEX_OBJ = $(BUILD_DIR)/lex.o
GRAM_OBJ = $(BUILD_DIR)/gram.o
UTIL_OBJ = $(BUILD_DIR)/util.o
LOCATION_OBJ = $(BUILD_DIR)/location.o
CLUMP_OBJ = $(BUILD_DIR)/clump.o
HEAP_OBJ = $(BUILD_DIR)/heap.o
STACK_OBJ = $(BUILD_DIR)/stack.o
OBJECT_OBJ = $(BUILD_DIR)/object.o
VALUE_OBJ = $(BUILD_DIR)/value.o
BLOCK_OBJ = $(BUILD_DIR)/block.o

XR0_OBJECTS = $(AST_OBJ) $(UTIL_OBJ) $(LEX_OBJ)
OBJECTS = $(XR0_OBJECTS) \
	  $(VALUE_OBJ) \
	  $(LOCATION_OBJ) \
	  $(BLOCK_OBJ) \
	  $(OBJECT_OBJ) \
	  $(CLUMP_OBJ) \
	  $(HEAP_OBJ) \
	  $(STACK_OBJ)

GRAM_TAB_C = $(BUILD_DIR)/gram.tab.c
GRAM_TAB_H = $(BUILD_DIR)/gram.tab.h
LEX_YY_C = $(BUILD_DIR)/lex.yy.c

$(TEST): $(MAIN_OBJ) $(BIN_DIR)
	@printf 'CC\t$@\n'
	@$(CC) $(CFLAGS) -o $@ $(MAIN_OBJ) $(OBJECTS)

test: $(TEST)
	@valgrind $(TEST)

$(BIN_DIR):
	@mkdir -p $(BIN_DIR)

$(MAIN_OBJ): test.c $(OBJECTS)
	@printf 'CC\t$@\n'
	@$(CC) $(CFLAGS) -o $@ -c test.c

$(STACK_OBJ): stack.c $(BLOCK_OBJ)
	@printf 'CC\t$@\n'
	@$(CC) $(CFLAGS) -o $@ -c stack.c

$(HEAP_OBJ): heap.c $(BLOCK_OBJ)
	@printf 'CC\t$@\n'
	@$(CC) $(CFLAGS) -o $@ -c heap.c

$(CLUMP_OBJ): clump.c $(BLOCK_OBJ)
	@printf 'CC\t$@\n'
	@$(CC) $(CFLAGS) -o $@ -c clump.c

$(BLOCK_OBJ): block.c $(OBJECT_OBJ)
	@printf 'CC\t$@\n'
	@$(CC) $(CFLAGS) -o $@ -c block.c

$(OBJECT_OBJ): object.c $(VALUE_OBJ)
	@printf 'CC\t$@\n'
	@$(CC) $(CFLAGS) -o $@ -c object.c

$(VALUE_OBJ): value.c $(LOCATION_OBJ)
	@printf 'CC\t$@\n'
	@$(CC) $(CFLAGS) -o $@ -c value.c

$(LOCATION_OBJ): location.c $(UTIL_OBJ)
	@printf 'CC\t$@\n'
	@$(CC) $(CFLAGS) -o $@ -c location.c

$(UTIL_OBJ): $(UTIL_DIR)/util.c $(BUILD_DIR)
	@printf 'CC\t$@\n'
	@$(CC) $(CFLAGS) -o $@ -c $(UTIL_DIR)/util.c

$(AST_OBJ): $(AST_DIR)/ast.c $(BUILD_DIR) $(LEX_OBJ)
	@printf 'CC\t$@\n'
	@$(CC) $(CFLAGS) -o $@ -c $(AST_DIR)/ast.c

$(LEX_OBJ): $(LEX_YY_C) $(BUILD_DIR)
	@printf 'CC\t$@\n'
	@$(CC) $(CFLAGS) -o $@ -c $(LEX_YY_C)

$(LEX_YY_C): $(XR0_INCLUDE_DIR)/lex.h $(LEX_DIR)/lex.l $(GRAM_OBJ)
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

clean:
	@rm -rf $(BUILD_DIR) $(BIN_DIR) $(OBJECTS)
