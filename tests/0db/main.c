#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/wait.h>
#include <errno.h>
#include <assert.h>

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
		
		/* close pipe file descriptors */
		close(to_child[0]);
		close(to_child[1]);
		close(from_child[0]);
		close(from_child[1]);

		/* execute Xr0 */
		char *args[] = { XR0, "-I", LIBX, "-d", TEST, (char *) NULL };
		int err = execvp(args[0], args);
		assert(!err);

		if (execvp(XR0, args) == -1) {
			fprintf(stderr, "execvp failed");
			exit(EXIT_FAILURE);
		}
	} else { /* parent process */
		printf("in parent process...\n");

		/* close unused ends of pipes in parent */
		close(to_child[0]); /* parent doesn't read from to_child */
		close(from_child[1]); /* parent doesn't write to from_child */

		/* send input to child */
		char *input = "step\n";
		write(to_child[1], input, strlen(input));

		/* close write to child */
		close(to_child[1]);

		/* buffer to read output from child */
		char buffer[MAX_BUF];
		ssize_t nbytes;

		while ((nbytes = read(from_child[0], buffer, sizeof(buffer) - 1) > 0)) {
			/* keep reading */
		}

		printf("output from bin/0v: %s\n", buffer);
		buffer[nbytes] = '\0'; /* null terminate string */

		/* close read end */
		close(from_child[0]);

		wait(NULL);
	}

	/* errors */
	int err = errno;
	printf("errno: %d\n", err);

	printf("debugger tests finished\n");
	return 0;
}
