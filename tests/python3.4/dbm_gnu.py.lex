(LIT "Provide the _gdbm module as a dbm submodule.")
(NEWLINE)
(KEYWORD try)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD from)
(ID "_gdbm")
(KEYWORD import)
(PUNCT "*")
(NEWLINE)
(DEDENT)
(KEYWORD except)
(ID "ImportError")
(KEYWORD as)
(ID "msg")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD raise)
(ID "ImportError")
(PUNCT "(")
(ID "str")
(PUNCT "(")
(ID "msg")
(PUNCT ")")
(PUNCT "+")
(LIT ", please install the python3-gdbm package")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ENDMARKER)
