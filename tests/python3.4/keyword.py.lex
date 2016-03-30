(LIT "Keywords (from \"graminit.c\")\n\nThis file is automatically generated; please don't muck it up!\n\nTo update the symbols in this file, 'cd' to the top directory of\nthe python source tree after building the interpreter and run:\n\n    ./python Lib/keyword.py\n")
(NEWLINE)
(ID "__all__")
(PUNCT "=")
(PUNCT "[")
(LIT "iskeyword")
(PUNCT ",")
(LIT "kwlist")
(PUNCT "]")
(NEWLINE)
(ID "kwlist")
(PUNCT "=")
(PUNCT "[")
(LIT "False")
(PUNCT ",")
(LIT "None")
(PUNCT ",")
(LIT "True")
(PUNCT ",")
(LIT "and")
(PUNCT ",")
(LIT "as")
(PUNCT ",")
(LIT "assert")
(PUNCT ",")
(LIT "break")
(PUNCT ",")
(LIT "class")
(PUNCT ",")
(LIT "continue")
(PUNCT ",")
(LIT "def")
(PUNCT ",")
(LIT "del")
(PUNCT ",")
(LIT "elif")
(PUNCT ",")
(LIT "else")
(PUNCT ",")
(LIT "except")
(PUNCT ",")
(LIT "finally")
(PUNCT ",")
(LIT "for")
(PUNCT ",")
(LIT "from")
(PUNCT ",")
(LIT "global")
(PUNCT ",")
(LIT "if")
(PUNCT ",")
(LIT "import")
(PUNCT ",")
(LIT "in")
(PUNCT ",")
(LIT "is")
(PUNCT ",")
(LIT "lambda")
(PUNCT ",")
(LIT "nonlocal")
(PUNCT ",")
(LIT "not")
(PUNCT ",")
(LIT "or")
(PUNCT ",")
(LIT "pass")
(PUNCT ",")
(LIT "raise")
(PUNCT ",")
(LIT "return")
(PUNCT ",")
(LIT "try")
(PUNCT ",")
(LIT "while")
(PUNCT ",")
(LIT "with")
(PUNCT ",")
(LIT "yield")
(PUNCT ",")
(PUNCT "]")
(NEWLINE)
(ID "iskeyword")
(PUNCT "=")
(ID "frozenset")
(PUNCT "(")
(ID "kwlist")
(PUNCT ")")
(PUNCT ".")
(ID "__contains__")
(NEWLINE)
(KEYWORD def)
(ID "main")
(PUNCT "(")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD import)
(ID "sys")
(PUNCT ",")
(ID "re")
(NEWLINE)
(ID "args")
(PUNCT "=")
(ID "sys")
(PUNCT ".")
(ID "argv")
(PUNCT "[")
(LIT 1)
(PUNCT ":")
(PUNCT "]")
(NEWLINE)
(ID "iptfile")
(PUNCT "=")
(ID "args")
(KEYWORD and)
(ID "args")
(PUNCT "[")
(LIT 0)
(PUNCT "]")
(KEYWORD or)
(LIT "Python/graminit.c")
(NEWLINE)
(KEYWORD if)
(ID "len")
(PUNCT "(")
(ID "args")
(PUNCT ")")
(PUNCT ">")
(LIT 1)
(PUNCT ":")
(ID "optfile")
(PUNCT "=")
(ID "args")
(PUNCT "[")
(LIT 1)
(PUNCT "]")
(NEWLINE)
(KEYWORD else)
(PUNCT ":")
(ID "optfile")
(PUNCT "=")
(LIT "Lib/keyword.py")
(NEWLINE)
(KEYWORD with)
(ID "open")
(PUNCT "(")
(ID "optfile")
(PUNCT ",")
(ID "newline")
(PUNCT "=")
(LIT "")
(PUNCT ")")
(KEYWORD as)
(ID "fp")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "format")
(PUNCT "=")
(ID "fp")
(PUNCT ".")
(ID "readlines")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "nl")
(PUNCT "=")
(ID "format")
(PUNCT "[")
(LIT 0)
(PUNCT "]")
(PUNCT "[")
(ID "len")
(PUNCT "(")
(ID "format")
(PUNCT "[")
(LIT 0)
(PUNCT "]")
(PUNCT ".")
(ID "strip")
(PUNCT "(")
(PUNCT ")")
(PUNCT ")")
(PUNCT ":")
(PUNCT "]")
(KEYWORD if)
(ID "format")
(KEYWORD else)
(LIT "\n")
(NEWLINE)
(KEYWORD with)
(ID "open")
(PUNCT "(")
(ID "iptfile")
(PUNCT ")")
(KEYWORD as)
(ID "fp")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "strprog")
(PUNCT "=")
(ID "re")
(PUNCT ".")
(ID "compile")
(PUNCT "(")
(LIT "\"([^\"]+)\"")
(PUNCT ")")
(NEWLINE)
(ID "lines")
(PUNCT "=")
(PUNCT "[")
(PUNCT "]")
(NEWLINE)
(KEYWORD for)
(ID "line")
(KEYWORD in)
(ID "fp")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(LIT "{1, \"")
(KEYWORD in)
(ID "line")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "match")
(PUNCT "=")
(ID "strprog")
(PUNCT ".")
(ID "search")
(PUNCT "(")
(ID "line")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(ID "match")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "lines")
(PUNCT ".")
(ID "append")
(PUNCT "(")
(LIT "        '")
(PUNCT "+")
(ID "match")
(PUNCT ".")
(ID "group")
(PUNCT "(")
(LIT 1)
(PUNCT ")")
(PUNCT "+")
(LIT "',")
(PUNCT "+")
(ID "nl")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(DEDENT)
(DEDENT)
(ID "lines")
(PUNCT ".")
(ID "sort")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(KEYWORD try)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "start")
(PUNCT "=")
(ID "format")
(PUNCT ".")
(ID "index")
(PUNCT "(")
(LIT "#--start keywords--")
(PUNCT "+")
(ID "nl")
(PUNCT ")")
(PUNCT "+")
(LIT 1)
(NEWLINE)
(ID "end")
(PUNCT "=")
(ID "format")
(PUNCT ".")
(ID "index")
(PUNCT "(")
(LIT "#--end keywords--")
(PUNCT "+")
(ID "nl")
(PUNCT ")")
(NEWLINE)
(ID "format")
(PUNCT "[")
(ID "start")
(PUNCT ":")
(ID "end")
(PUNCT "]")
(PUNCT "=")
(ID "lines")
(NEWLINE)
(DEDENT)
(KEYWORD except)
(ID "ValueError")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "sys")
(PUNCT ".")
(ID "stderr")
(PUNCT ".")
(ID "write")
(PUNCT "(")
(LIT "target does not contain format markers\n")
(PUNCT ")")
(NEWLINE)
(ID "sys")
(PUNCT ".")
(ID "exit")
(PUNCT "(")
(LIT 1)
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD with)
(ID "open")
(PUNCT "(")
(ID "optfile")
(PUNCT ",")
(LIT "w")
(PUNCT ",")
(ID "newline")
(PUNCT "=")
(LIT "")
(PUNCT ")")
(KEYWORD as)
(ID "fp")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "fp")
(PUNCT ".")
(ID "writelines")
(PUNCT "(")
(ID "format")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD if)
(ID "__name__")
(PUNCT "==")
(LIT "__main__")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "main")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ENDMARKER)
