# Esterel in Racket

![ci status](https://github.com/rfindler/esterel/actions/workflows/ci.yaml/badge.svg?branch=main)

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

## Documentation

The documentation is available locally after installation via DrRacket's help menu (search for "Esterel") or via the command-line (run `raco docs esterel`). The documentation is [also available online](https://docs.racket-lang.org/esterel/).
