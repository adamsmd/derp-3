(LIT "Fixer that turns 'long' into 'int' everywhere.\n")
(NEWLINE)
(KEYWORD from)
(ID "lib2to3")
(KEYWORD import)
(ID "fixer_base")
(NEWLINE)
(KEYWORD from)
(ID "lib2to3")
(PUNCT ".")
(ID "fixer_util")
(KEYWORD import)
(ID "is_probably_builtin")
(NEWLINE)
(KEYWORD class)
(ID "FixLong")
(PUNCT "(")
(ID "fixer_base")
(PUNCT ".")
(ID "BaseFix")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "BM_compatible")
(PUNCT "=")
(KEYWORD True)
(NEWLINE)
(ID "PATTERN")
(PUNCT "=")
(LIT "'long'")
(NEWLINE)
(KEYWORD def)
(ID "transform")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "node")
(PUNCT ",")
(ID "results")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "is_probably_builtin")
(PUNCT "(")
(ID "node")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "node")
(PUNCT ".")
(ID "value")
(PUNCT "=")
(LIT "int")
(NEWLINE)
(ID "node")
(PUNCT ".")
(ID "changed")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(DEDENT)
(ENDMARKER)
