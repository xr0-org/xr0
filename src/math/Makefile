# dirs
BIN_DIR = bin
BUILD_DIR = build
XR0_INCLUDE_DIR = ../../include
SRC_DIR = ..
UTIL_DIR = $(SRC_DIR)/util

# executable
TEST = $(BIN_DIR)/test

# commands
CC = gcc
CFLAGS = -I $(XR0_INCLUDE_DIR) -I . -Wall -g -Wreturn-type -std=gnu11

# build artifacts
MAIN_OBJ = $(BUILD_DIR)/test.o
UTIL_OBJ = $(BUILD_DIR)/util.o
MATH_OBJ = $(BUILD_DIR)/math.o

XR0_OBJECTS = $(UTIL_OBJ)
OBJECTS = $(XR0_OBJECTS) \
	  $(MATH_OBJ)

$(TEST): $(MAIN_OBJ) $(BIN_DIR)
	@printf 'CC\t$@\n'
	@$(CC) $(CFLAGS) -o $@ $(MAIN_OBJ) $(OBJECTS)

test: $(TEST)
	@valgrind $(TEST)

test-verbose: $(TEST)
	@valgrind --leak-check=full $(TEST)

$(BIN_DIR):
	@mkdir -p $(BIN_DIR)

$(MAIN_OBJ): test.c $(OBJECTS)
	@printf 'CC\t$@\n'
	@$(CC) $(CFLAGS) -o $@ -c test.c

$(MATH_OBJ): math.c $(XR0_INCLUDE_DIR)/math.h
	@printf 'CC\t$@\n'
	@$(CC) $(CFLAGS) -o $@ -c math.c

$(UTIL_OBJ): $(UTIL_DIR)/util.c $(BUILD_DIR)
	@printf 'CC\t$@\n'
	@$(CC) $(CFLAGS) -o $@ -c $(UTIL_DIR)/util.c

$(BUILD_DIR):
	@mkdir -p $(BUILD_DIR)

clean:
	@rm -rf $(BUILD_DIR) $(BIN_DIR) $(OBJECTS)
