#lang racket

(require racket/pretty)

(require (submod "derp.rkt" graphvis))
(require "derp.rkt")
(require "util/benchmark.rkt")

;;;; Python grammar

(define (mk-tag? tag-name)
  (lambda (token)
    (eq? (car token) tag-name)))

(define (mk-value? predicate)
  (lambda (token)
    (predicate (cadr token))))

(define NAME      (lang (@--> (token (mk-tag? 'ID) 'NAME)
                              (λ (_ id) id))))
(define NUMBER    (lang (@--> (token (and/c (mk-tag? 'LIT) (mk-value? number?)) 'NUMBER)
                              (λ (_ num) num))))
(define STRING    (lang (@--> (token (and/c (mk-tag? 'LIT) (mk-value? (lambda (x) (or (string? x) (bytes? x))))) 'STRING)
                              (λ (_ str) str))))
(define NEWLINE   (lang (@--> (token (mk-tag? 'NEWLINE) 'NEWLINE)
                              (λ (_) #f))))
(define ENDMARKER (lang (@--> (token (mk-tag? 'ENDMARKER) 'ENDMARKER)
                              (λ (_) #f))))
(define INDENT    (lang (@--> (token (mk-tag? 'INDENT) 'INDENT)
                              (λ (_) #f))))
(define DEDENT    (lang (@--> (token (mk-tag? 'DEDENT) 'DEDENT)
                              (λ (_) #f))))

(define python-literal->language
 (λ (x)
   (match x
     [(or "+" "-" "*" "**" "/" "//" "%"
          "<<" ">>" "&" "|" "^" "~"
          "<" ">" "<=" ">=" "==" "!="
          "(" ")" "[" "]" "{" "}"
          "," ":" "." ";" "@" "="
          "+=" "-=" "*=" "/=" "//=" "%="
          "&=" "|=" "^=" ">>=" "<<=" "**="
          "..." ; not in lexical spec
          "<>" ; not in lexical spec
          "->" ; not in lexical spec
          )
      (lang (>--> (token (and/c (mk-tag? 'PUNCT) (mk-value? (λ (v) (equal? v x)))) x)
                  [`(PUNCT ,k) k]))]

     [(or "False" "None" "True" "and" "as" "assert" "break"
          "class" "continue" "def" "del" "elif" "else" "except"
          "finally" "for" "from" "global" "if" "import" "in"
          "is" "lambda" "nonlocal" "not" "or" "pass" "raise"
          "return" "try" "while" "with" "yield")
      (lang (>--> (token (and/c (mk-tag? 'KEYWORD) (mk-value? (λ (v) (equal? x (symbol->string v))))) x)
                  [`(KEYWORD ,k) (symbol->string k)]))]

     ['NAME NAME]
     ['NUMBER NUMBER]
     ['STRING STRING]
     ['NEWLINE NEWLINE]
     ['ENDMARKER ENDMARKER]
     ['INDENT INDENT]
     ['DEDENT DEDENT]

     [else
      (printf "unknown literal: ~s~n" x)
      (error "unknown literal")])))

(set-literal->language! python-literal->language)

(define py-lang (grammar-from-cfg-file file_input "python.yacc.sx"))

(optimize py-lang)

(define (token-generator-list l)
  (lambda ()
    (let ([x (car l)])
      (set! l (cdr l))
      x)))

(define (usage)
  (printf "usage:\n")
  (printf "  racket pyparse.rkt parse [file]\n")
  (printf "  racket pyparse.rkt graphviz [file]\n")
  (printf "  racket pyparse.rkt bench <rounds> <drops> <min-cpu-time> [file]\n")
  (printf "  racket pyparse.rkt help\n")
  (printf "\n")
  (printf "Standard input is used if [file] is omitted\n"))

(define (usage-error)
  (usage)
  (exit 1))

(define (match-file-or-stdin file)
  (match file
   [(list) (list "<stdin>" (current-input-port))]
   [(list (var file))
    (with-handlers ([exn:fail:filesystem?
                     (lambda (e)
                       (printf "Error: could not open file: ~s~n" file)
                       (usage-error))])
       (list file (open-input-file file)))]
   [else (usage-error)]))

(define-match-expander file-or-stdin
  (lambda (stx)
    (syntax-case stx ()
      [(_ filename port) #'(app match-file-or-stdin (list (var filename) (var port)))])))

(match (vector->list (current-command-line-arguments))
 [(list-rest (and (var mode) (or "parse" "graphviz")) (file-or-stdin filename port))
  (define graphviz (match mode
                     ["parse" #f]
                     ["graphviz" #t]))
  (define (graphviz-action l c)
    (printf "// Before token ~s~n" c)
    (dotify l))
  (define lines (token-generator-list (append (read-all port) (list eof))))
  (define action (if graphviz graphviz-action values))
  (define forest (parse py-lang lines action))
  (unless graphviz
    (cond
     [(= (set-count forest) 0) (pretty-write #f)]
     [(= (set-count forest) 1) (pretty-write (set-first forest))]
     [else
      (printf "; ambiguous parse:~n")
      (pretty-write forest)]))]

 [(list-rest "bench" (var rounds-str) (var drops-str) (var min-cpu-time-str) (file-or-stdin filename port))
  (let ([rounds (string->number rounds-str)]
        [drops (string->number drops-str)]
        [min-cpu-time (string->number min-cpu-time-str)])
    (when (not (and (exact-integer? rounds)
                    (exact-integer? drops)
                    (number? min-cpu-time)))
          (usage-error))

    (define lines (append (read-all port) (list eof)))
    (benchmark rounds drops min-cpu-time
      (lambda ()
        ;; the maps here ensure we have fresh cons cells that are not cached
        (parse py-lang (token-generator-list (map (lambda (x) (if (pair? x) (map (lambda (x) x) x) x)) lines))))))]

 [(list "help")
  (usage)
  (printf "~n")
  (printf "Modes:~n")
  (printf " - 'parse' parses the input and then prints the resulting parse forest.~n")
  (printf " - 'graphviz' parses the input and prints a Graphviz dot graph at each step of the parse.~n")
  (printf " - 'bench' benchmarks the performance of the code.~n")
  (printf "      The benchmark is looped so it takes at least <min-cpu-time>.~n")
  (printf "      Then, that loop is run <rounds> times.  The mean is then computed~n")
  (printf "      both with and without the highest and lowest <drops> number of results.~n")]

 [else (usage-error)])

(exit)
