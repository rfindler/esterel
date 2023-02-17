# Esterel in Racket

This is a version of Esterel that tries to cooperate as much of the
Racket programming language as possible. For example, there is no
`seq` just use `begin`. Similarly, no `loop`; just use functions or
`for` in the usual way. Much of the language is just fine but in order
to have instanteous reaction to absense, continuations are used (to
implement `Can`).

