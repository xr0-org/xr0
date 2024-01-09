# dirs
BIN_DIR = ../../bin
BUILD_DIR = ../../build/0c
INCLUDE_DIR = ../../include

# commands
CC = gcc -g -Wreturn-type -std=gnu11
CFLAGS = -Wall -I $(INCLUDE_DIR)
VALGRIND = valgrind

XR0C = $(BIN_DIR)/0c

# build artifacts
MAIN_OBJ = $(BUILD_DIR)/0c.o

$(XR0C): $(MAIN_OBJ) $(BIN_DIR)
	@printf 'CC\t$@\n'
	@$(CC) $(CFLAGS) -o $@ $(MAIN_OBJ) $(OBJECTS)

check: $(XR0C)
	$(XR0C) ../../tests/3-program/000-matrix.x > file.c
	c89 file.c -o matrix
	./matrix

$(MAIN_OBJ): main.c $(BUILD_DIR)
	@printf 'CC\t$@\n'
	@$(CC) $(CFLAGS) -I $(BUILD_DIR) -o $@ -c main.c

$(BIN_DIR):
	@mkdir -p $(BIN_DIR)

$(BUILD_DIR):
	@mkdir -p $(BUILD_DIR)

clean:
	@rm -rf $(BUILD_DIR) $(BIN_DIR) $(OBJECTS)
