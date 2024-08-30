#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/wait.h>
#include <errno.h>
#include <assert.h>
#include <fcntl.h>

#define XR0 "./../../bin/0v"
#define LIBX "./../../libx"
#define TEST "00-basic/test.x"
#define TEST_CFG "00-basic/test.cfg"
#define MAX_BUF 10240 /* 10 KB, states can be big... */

#define MAX_LINE_LEN 256

struct instr;

struct instr {
	char *cmd;
	char *state;
};

char *
trim(char *str);

struct instr *
read_testcfg(const char *fname, int *icount);

int
main(int argc, char * argv[])
{
	printf("running debugger tests...\n");
	int icount;
	struct instr *instr = read_testcfg(TEST_CFG, &icount);
	for (int i = 0; i < icount; i++) {
		printf("[ %s, %s]\n", instr[i].cmd, instr[i].state);
	}

	int to_child[2];
	int from_child[2];

	if (pipe(to_child) == -1 || pipe(from_child) == -1) {
		fprintf(stderr, "pipe failed");
		exit(EXIT_FAILURE);
	}

	pid_t pid = fork();

	if (pid == 0) { /* child process */
		printf("in child process...\n");

		/* redirect stdin to read from read end of to_child */
		dup2(to_child[0], STDIN_FILENO);
		/* redirect stdout to write to the write end of from_child */
		dup2(from_child[1], STDOUT_FILENO);
		/* redirect stderr to capture error messages */
		dup2(from_child[1], STDERR_FILENO);
		
		/* close unused ends of pipes */
		close(to_child[1]);
		close(from_child[0]);

		/* execute Xr0 */
		char *args[] = { XR0, "-I", LIBX, "-d", TEST, (char *) NULL };

		if (execvp(XR0, args) == -1) {
			fprintf(stderr, "execvp failed\n");
			exit(EXIT_FAILURE);
		}
        	fflush(stdout);

	} else { /* parent process */
		printf("in parent process...\n");

		/* close unused ends of pipes */
		close(to_child[0]);
		close(from_child[1]);

		char buffer[MAX_BUF];
		ssize_t nbytes;

		/* read initial debugger info */
		usleep(100000); // Sleep for 100 ms

		nbytes = read(from_child[0], buffer, sizeof(buffer) - 1);
		if (nbytes > 0) {
			buffer[nbytes] = '\0';
			printf("read from child: %s\n", buffer);
		} else if (nbytes == -1 && errno == EAGAIN) {
			/* No data to read right now */
			usleep(100000); // Sleep for 100 ms
		} else if (nbytes == 0) {
			/* eof reached */
			printf("end of output from child\n");
		} else {
			fprintf(stderr, "read failed\n");
		}
		printf("read %ld bytes from bin/0v:\n%s\n", nbytes, buffer);

		const char *commands[] = {
			"step\n", "next\n", "quit\n"
		};
		int ncommands = sizeof(commands) / sizeof(commands[0]);
		for (int i = 0; i < ncommands; i++) {
			/* write command to child process */
			ssize_t bytes_written = write(to_child[1], commands[i], strlen(commands[i]));
			if (bytes_written == -1) {
				fprintf(stderr, "write failed\n");
				break;
			}
			fsync(to_child[1]);
			printf("sent command: %s\n", commands[i]);

			/* read output from child process */
			sleep(1);
			nbytes = read(from_child[0], buffer, sizeof(buffer) - 1);
			if (nbytes > 0) {
				buffer[nbytes] = '\0';
			} else if (nbytes == -1 && errno == EAGAIN) {
				/* No data to read right now */
				usleep(100000); // Sleep for 100 ms
			} else if (nbytes == 0) {
				/* eof */
				printf("end of output from child\n");
			} else {
				fprintf(stderr, "read failed\n");
			}
			printf("read %ld bytes from bin/0v:\n%s\n", nbytes, buffer);
		}

		close(to_child[1]);
		close(from_child[0]);

		int status;
		if (waitpid(pid, &status, 0) == -1) {
			fprintf(stderr, "wait failed\n");
			exit(EXIT_FAILURE);
		} else {
			if (WIFEXITED(status)) {
				printf("child exited with status %d\n", WEXITSTATUS(status));
			} else if (WIFSIGNALED(status)) {
				printf("child killed by signal %d\n", WTERMSIG(status));
			}
		}
	}

	/* errors */
	printf("errno: %d\n", errno);

	printf("debugger tests finished\n");

	return 0;
}

char *
trim(char *str)
{
	char *end;

	while (*str == ' ') str++; /* grow head */
	end = str + strlen(str) - 1;
	while (end > str && (*end == ' ')) end--; /* shrink end */
	*(end + 1) = '\0';
	return str;
}

struct instr *
read_testcfg(const char *fname, int *icount)
{
	FILE *f = fopen(fname, "r");
	if (f == NULL) {
		perror("error opening file");
		return NULL;	
	}

	char line[MAX_LINE_LEN];
	struct instr *instructions = NULL;
	*icount = 0;

	while(fgets(line, sizeof(line), f)) {
		line[strcspn(line, "\n")] = 0;

		char *cmd = strtok(line, ":");
		char *state = strtok(NULL, ":");
		
		instructions = realloc(instructions, sizeof(struct instr) * (*icount + 1));
		if (instructions == NULL) {
			perror("error in memory allocation");
			return NULL;
		}
		
		instructions[*icount].cmd = malloc(strlen(cmd) + 1);
		instructions[*icount].state = malloc(strlen(state) + 1);
		strcpy(instructions[*icount].cmd, cmd);	
		strcpy(instructions[*icount].state, state);

		(*icount)++;
	}
	fclose(f);
	return instructions;
}
