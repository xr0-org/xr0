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
#define TEST "debugger-test.x"
#define MAX_BUF 10240 /* 10 KB, states can be big... */

int
main(int argc, char * argv[])
{
	printf("running debugger tests...\n");

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
	} else { /* parent process */
		printf("in parent process...\n");

		/* close unused ends of pipes */
		close(to_child[0]);
		close(from_child[1]);

		char buffer[MAX_BUF];
		ssize_t nbytes;

		/* read initial debugger info */
		nbytes = read(from_child[0], buffer, sizeof(buffer) - 1);
		printf("initial read %ld bytes from bin/0v:\n\n%s\n\n", nbytes, buffer);
		if (nbytes == -1) {
			fprintf(stderr, "read failed\n");
			exit(EXIT_FAILURE);
		} else if (nbytes == 0) {
			printf("no output from bin/0v\n");
		}

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
			while (1) {
				sleep(1);
				nbytes = read(from_child[0], buffer, sizeof(buffer) - 1);
				if (nbytes == -1) {
					fprintf(stderr, "read failed");
					exit(EXIT_FAILURE);
				} else if (nbytes == 0) {
					printf("end of file\n");
					break;
				}
				
				buffer[nbytes] = '\0';
				printf("read %ld bytes from bin/0v:\n%s\n", nbytes, buffer);
				fflush(stdout);
				break;
			}
		}


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
	int err = errno;
	printf("errno: %d\n", err);

	printf("debugger tests finished\n");

	return 0;
}
