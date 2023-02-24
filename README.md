# Esterel in Racket

This is a version of Esterel that tries to cooperate as much of the
Racket programming language as possible. For example, there is no
`seq` just use `begin`. Most of Racket is fine to use but in order
to have instanteous reaction to absense, continuations are used (to
implement `Can`), so programs that use state will probably not behave 
correctly.

## Installation
To install, first install [Racket](https://racket-lang.org/). 

Then, either using DrRacket, choose the "File|Install Package..." menu item and enter `esterel` as the "Package Source"

Or, using the command-line
```sh
raco pkg install esterel
```
