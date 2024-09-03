.POSIX:

# commands
CC = gcc
CFLAGS = -g -I src/include -std=gnu11 -Werror -Wreturn-type -Wall
VALGRIND = valgrind --fullpath-after=`pwd`/src/
LEX = lex
YACC = bison -yvd

# dirs
BIN_DIR = bin
BUILD_DIR = build
SRC_DIR = src
TEST_DIR = tests

DEPS_MK = scripts/deps.mk

# executable
XR0V = $(BIN_DIR)/0v

main: $(XR0V)

include $(DEPS_MK)

$(XR0V): $(BIN_DIR) $(HEADERS) $(OBJECTS) parser
	@printf 'CC\t$@\n'
	@$(CC) $(CFLAGS) -o $@ $(OBJECTS)

AST_DIR = $(SRC_DIR)/ast

PARSER_JUNK = $(GRAM_TAB_H) $(GRAM_TAB_C) $(LEX_YY_C) $(AST_DIR)/gram.output
GRAM_TAB_H = $(SRC_DIR)/include/gram.tab.h
GRAM_TAB_C = $(AST_DIR)/gram.tab.c
LEX_YY_C = $(AST_DIR)/lex.yy.c

parser: $(LEX_YY_C) $(GRAM_TAB_H) $(GRAM_TAB_C)

$(LEX_YY_C): $(AST_DIR)/lex.l
	@printf 'LEX\t$@\n'
	@$(LEX) -o $@ $(AST_DIR)/lex.l

$(GRAM_TAB_C) $(GRAM_TAB_H): $(AST_DIR)/gram.y
	@printf 'YACC\t$@\n'
	@$(YACC) -o $(GRAM_TAB_C) $(AST_DIR)/gram.y
	@mv $(AST_DIR)/gram.tab.h $(GRAM_TAB_H)

$(BIN_DIR):
	@mkdir -p $(BIN_DIR)

# tests

test: $(XR0V) $(TEST_DIR)
	@./tests/run

check: $(RUNTEST) $(XR0V)
	$(VALGRIND) $(XR0V) -I libx $(filter-out $@,$(MAKECMDGOALS))

check-verbose: $(RUNTEST) $(XR0V)
	$(VALGRIND) --num-callers=30 \
		$(XR0V) -v -I libx $(filter-out $@,$(MAKECMDGOALS))

debug: $(RUNTEST) $(XR0V)
	$(VALGRIND) $(XR0V) -d -I libx $(filter-out $@,$(MAKECMDGOALS))

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
		$(XR0V) -I libx tests/99-program/100-lex/parse.x

lex-verbose: $(XR0V) 
	$(VALGRIND) --num-callers=30 \
		$(XR0V) -I libx -v tests/99-program/100-lex/parse.x

matrix: $(XR0V)
	$(VALGRIND) $(XR0V) -I libx tests/99-program/000-matrix.x

matrix-leaks: $(XR0V)
	$(VALGRIND) --leak-check=full \
		$(XR0V) -I libx tests/99-program/000-matrix.x

matrix-verbose: $(XR0V) 
	$(VALGRIND) --num-callers=30 \
		$(XR0V) -I libx tests/99-program/000-matrix.x


clean:
	@rm -rf $(BUILD_DIR) $(BIN_DIR) $(PARSER_JUNK) $(DEPS_MK) Makefile
