#include<stdlib.h>
#include<stdio.h>
#include<string.h>
#include<stdbool.h>
#include<assert.h>

#include "breakpoint.h"
#include "util.h"
#include "lex.h"

#define MAX_BREAKPOINTS 100

/* XXX: validation of filenames and lengths for breakpoints */

struct breakpoint {
	bool enabled;
	bool reached;
	// char *filename;
	int linenumber;
};

struct breakpoint breakpoints[MAX_BREAKPOINTS];
int breakpoint_count = 0;

static char *
breakpoint_str(struct breakpoint);

char *
breakpoint_list()
{
	struct strbuilder *b = strbuilder_create();
	strbuilder_printf(b, "Num\tLine\n");
	for (int i = 0; i < breakpoint_count; i++) {
		struct breakpoint bp = breakpoints[i];
		strbuilder_printf(b, "%d\t%s\n", i, breakpoint_str(bp));
	}
	return strbuilder_build(b);
}

static char *
breakpoint_str(struct breakpoint bp)
{
	if (bp.enabled) {
		struct strbuilder *b = strbuilder_create();
		strbuilder_printf(b, "%d", bp.linenumber);
		return strbuilder_build(b);
	}
	return dynamic_str("");
}

static int
breakpoint_exists(struct breakpoint);

struct error *
breakpoint_set(char *filename, int linenumber)
{
	struct breakpoint bp = (struct breakpoint) {
		.enabled = true, .linenumber = linenumber
	};
	if (breakpoint_count > MAX_BREAKPOINTS) {
		return error_printf("Maximum number of breakpoints reached\n");
	}
	int id = breakpoint_exists(bp);
	if (id != -1) {
		return error_printf(
			"Note: breakpoint %s is already set as %d\n", breakpoint_str(bp), id
		);
	}
	breakpoints[breakpoint_count] = bp;
	breakpoint_count++;
	return NULL;
}

static bool
breakpoint_equal(struct breakpoint bp1, struct breakpoint bp2);

static int
breakpoint_exists(struct breakpoint bp)
{
	for (int i = 0; i < breakpoint_count; i++) {
		if (breakpoint_equal(bp, breakpoints[i]) && !breakpoints[i].reached) {
			breakpoints[i].reached = true;
			return i;
		}
	}
	return -1;
}

static bool
breakpoint_equal(struct breakpoint bp1, struct breakpoint bp2)
{
	return bp1.linenumber == bp2.linenumber;
}

struct error *
breakpoint_delete(int id)
{
	if (breakpoints[id].enabled) {
		breakpoints[id].enabled = false;
		return NULL;
	}
	return error_printf("No breakpoint with id: %d", id);
}

bool
breakpoint_shouldbreak(struct lexememarker *loc)
{
	// char *fname = lexememarker_filename(loc);
	int linenum = lexememarker_linenum(loc);

	struct breakpoint bp = (struct breakpoint) {
		.linenumber = linenum,
	};
	int index = breakpoint_exists(bp);
	if (index == -1) {
		return false;
	}
	return true;
}

void
breakpoint_reset()
{
	for (int i = 0; i < breakpoint_count; i++) {
		breakpoints[i].reached = false;
	}
}
