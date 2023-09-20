CC = gcc -g -Wreturn-type -std=gnu11
LEX = lex
YACC = bison -yvd

BIN = bin
XR0V = $(BIN)/0v

TESTDIR = tests
RUNTEST = $(TESTDIR)/run
TESTFILES = $(shell find $(TESTDIR) -name '*.0')

OBJECTS = ast.o lex.o gram.o state.o util.o verify.o

$(XR0V): main.o $(BIN)
	@printf 'CC\t$@\n'
	@$(CC) -o $@ main.o $(OBJECTS)

$(BIN):
	@mkdir -p $(BIN)

main.o: main.c $(OBJECTS)
	@printf 'CC\t$@\n'
	@$(CC) -c main.c

util.o: util.c util.h ast.o
	@printf 'CC\t$@\n'
	@$(CC) -o $@ -c util.c

verify.o: verify.c verify.h state.o
	@printf 'CC\t$@\n'
	@$(CC) -o $@ -c verify.c

state.o: state.c state.h ast.o util.o
	@printf 'CC\t$@\n'
	@$(CC) -o $@ -c state.c

ast.o: ast.c ast.h
	@printf 'CC\t$@\n'
	@$(CC) -o $@ -c ast.c

lex.o: lex.yy.c
	@printf 'CC\t$@\n'
	@$(CC) -o $@ -c lex.yy.c

lex.yy.c: lex.h lex.l gram.o
	@printf 'LEX\t$@\n'
	@$(LEX) lex.l

gram.o: gram.tab.c gram.tab.h
	@printf 'CC\t$@\n'
	@$(CC) -o $@ -c gram.tab.c

gram.tab.c gram.tab.h: gram.y ast.h gram_util.h
	@printf 'YACC\t$@\n'
	@$(YACC) -b gram gram.y

test: $(RUNTEST) $(TESTFILES) $(XR0V) 
	@./tests/run

check: $(RUNTEST) $(TESTFILES) $(XR0V)
	valgrind $(XR0V) -I libx $(filter-out $@,$(MAKECMDGOALS))

check-verbose: $(RUNTEST) $(TESTFILES) $(XR0V)
	valgrind --num-callers=30 $(XR0V) -v -I libx $(filter-out $@,$(MAKECMDGOALS))

clean:
	@rm -rf $(BIN) $(OBJECTS) gram.output gram.tab.c gram.tab.h lex.yy.c *.gch
