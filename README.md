# dynamic-esterel

This is a version of Esterel that tries to re-use as much of the
"normal" programming language as possible. For example, there is no
`seq` just use `begin`. Similarly, no `loop`; just use functions or 
`for` in the usual way.

The difficulty is in `can`. To implement it we do a runtime search
(hence the name (but ideas for a better one are welcome)).

