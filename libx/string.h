#ifndef STRING_H
#define STRING_H

#include <stddef.h>

#define bool int

sfunc bool
strttd(char *s) [ /* (∃i)(i ≥ 0 && s[i] == '\0') */
	int i;
	result === (some (i = 0; ; i++) {
		s[i] == '\0';
	});
];

/* strlen: the assertion of the constancy of s is critical because it ensures us
 * that the postcondition is achieved by making the result value the length,
 * rather than by changing the string somehow. */
axiom size_t
strlen(const char *s) [
	int i;
	s[result] == '\0';
	for (i = 0; i < result; i++) {
		s[i] != '\0';
	}
];
/* XXX: how is the determinism or functional nature of strlen expressed? */

sfunc strttd
concat(strttd s, strttd t) [
	int i;
	for (i = 0; i < strlen(s); i++) {
		result[i] == s[i];
	}
	for (i = strlen(s); i <= strlen(s) + strlen(t); i++) {
		result[i] == t[i - strlen(s)];
	}
];

lemma
concat_strlen(strttd s, strttd t) [
	strlen(concat(s, t)) == strlen(s) + strlen(t);
]{
	int i;

l0:	concat(s, t)[strlen(s) + strlen(t)]
== { concat(s, t) }
	t[strlen(t)]
== { strlen(t) }
	'\0';

	/* strlen(concat(s, t)) ≤ strlen(s) + strlen(t) */
le:	strlen(concat(s, t)) > strlen(s) + strlen(t)
==> { strlen(concat(s, t)) }
	concat(s, t)[strlen(s) + strlen(t)] != '\0'
=== { l0 }
	false;

m0:	for (i = 0; i < strlen(s); i++) {
		concat(s, t)[i]
	== { concat(s, t) }
		s[i]
	!= { strlen(s) }
		'\0';
	}

strlen_t_shift:
	true
==> { strlen(t) }
	(for (i = 0; i < strlen(t); i++) {
		t[i] != '\0';
	})
==> { for_sub(i + strlen(s)) }
	(for (i = strlen(s); i < strlen(s) + strlen(t); i++) {
		t[i - strlen(s)] != '\0';
	});

m1:	for (i = strlen(s); i < strlen(s) + strlen(t); i++) {
		concat(s, t)[i]
	== { concat(s, t) }
		t[i - strlen(s)]
	!= { strlen_t_shift }
		'\0';
	}

	/* strlen(concat(s, t)) ≥ strlen(s) + strlen(t) */
ge:	m0 && m1
==> { for_union }
	(for (i = 0; i < strlen(s) + strlen(t); i++) {
		concat(s, t)[i] != '\0';
	})
==> { strlen(concat(s, t)) } /* from s[result] == '\0' */
	(for (i = 0; i < strlen(s) + strlen(t); i++) {
		strlen(concat(s, t)) != i;
	})
==> /* now arithmetic */
	strlen(concat(s, t)) >= strlen(s) + strlen(t);

	le && ge
==>	strlen(concat(s, t)) == strlen(s) + strlen(t);
}

#endif
