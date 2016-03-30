Derp 3
================

This is the third generation of the Derp parser, which is based on the
following work:

> Michael D. Adams, Celeste Hollenbeck, and Matt Might. "On the
> Complexity and Performance of Parsing with Derivatives." To appear in
> Proceedings of the 37th ACM SIGPLAN Conference on Programming
> Language Design and Implementation, PLDI '16. ACM, New York, NY,
> USA, June 2016.

License
----------------

This code is dual licensed either under the CRAPL or BSD licenses.
See LICENSE-CRAPL or LICENSE-BSD.

Requirements
----------------

This code was tested with Racket version 6.1.1, but it should work
with any recent version of Racket.

History
----------------

The first generation of Derp was based on parsing with derivatives and
the following paper.

> Matthew Might, David Darais and Daniel Spiewak. "Functional Pearl:
> Parsing with Derivatives." In Proceedings of the 16th ACM SIGPLAN
> international conference on Functional programming, ICFP '11. ACM,
> New York, NY, USA, September 2016. ISBN 978-1-4503-0865-6. doi:
> 10.1145/2034773.2034801.

Unfortunately, it was to slow for practical or even pedagogical use.

The second generation, Derp 2, used the same interface as Derp 1 but
internally was based on Racket's `parser-tools/cfg-parser`.

This version, Derp 3, goes back to using parsing with derivatives and
shows how they can be implemented efficiently.

Usage
----------------

```
usage:
  racket pyparse.rkt parse [file]
  racket pyparse.rkt graphviz [file]
  racket pyparse.rkt bench <rounds> <drops> <min-cpu-time> [file]
  racket pyparse.rkt help

Standard input is used if [file] is omitted

Modes:
 - 'parse' parses the input and then prints the resulting parse forest.
 - 'graphviz' parses the input and prints a Graphviz dot graph at each step of the parse.
 - 'bench' benchmarks the performance of the code.
      The benchmark is looped so it takes at least <min-cpu-time>.
      Then, that loop is run <rounds> times.  The mean is then computed
      both with and without the highest and lowest <drops> number of results.
```

Example
----------------

```
$ cd src
$ racket pyparse.rkt parse ../tests/python3.4/struct.py.lex
$ racket pyparse.rkt graphviz ../tests/python3.4/struct.py.lex
$ racket pyparse.rkt bench 10 1 1.0 ../tests/python3.4/struct.py.lex
```

Directory structure
------------------------

- `README.md`
- `LICENSE-BSD`
- `LICENSE-CRAPL`
- `src/` source code
  - `derp.rkt` parser library
  - `pyparse.rkt` Python parser
  - `python.yacc.sx` grammar for the Python parser
  - `util/` helper code
- `tests/` example inputs
  - `python3.4/` inputs from the Python library

Input format
----------------

Input is taken in already lexed form.
Each line of input is one token and
is one of the following.

- `(NAME <name>)` A Python identifier
- `(NUMBER <number>)` A numeric literal
- `(STRING <string>)` A string literal
- `(KEYWORD <keyword>)` A Python keyword
- `(PUNCT <punct>)` A punctuation based token (e.g., `(` or `)` in function calls)
- `(NEWLINE)` The newline token
- `(ENDMARKER)` End of all input
- `(INDENT)` Start of indentation block
- `(DEDENT)` End of indentation block
