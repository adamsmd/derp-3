(LIT "Fixer for exec.\n\nThis converts usages of the exec statement into calls to a built-in\nexec() function.\n\nexec code in ns1, ns2 -> exec(code, ns1, ns2)\n")
(NEWLINE)
(KEYWORD from)
(PUNCT ".")
(PUNCT ".")
(KEYWORD import)
(ID "pytree")
(NEWLINE)
(KEYWORD from)
(PUNCT ".")
(PUNCT ".")
(KEYWORD import)
(ID "fixer_base")
(NEWLINE)
(KEYWORD from)
(PUNCT ".")
(PUNCT ".")
(ID "fixer_util")
(KEYWORD import)
(ID "Comma")
(PUNCT ",")
(ID "Name")
(PUNCT ",")
(ID "Call")
(NEWLINE)
(KEYWORD class)
(ID "FixExec")
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
(LIT "\n    exec_stmt< 'exec' a=any 'in' b=any [',' c=any] >\n    |\n    exec_stmt< 'exec' (not atom<'(' [any] ')'>) a=any >\n    ")
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
(KEYWORD assert)
(ID "results")
(NEWLINE)
(ID "syms")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "syms")
(NEWLINE)
(ID "a")
(PUNCT "=")
(ID "results")
(PUNCT "[")
(LIT "a")
(PUNCT "]")
(NEWLINE)
(ID "b")
(PUNCT "=")
(ID "results")
(PUNCT ".")
(ID "get")
(PUNCT "(")
(LIT "b")
(PUNCT ")")
(NEWLINE)
(ID "c")
(PUNCT "=")
(ID "results")
(PUNCT ".")
(ID "get")
(PUNCT "(")
(LIT "c")
(PUNCT ")")
(NEWLINE)
(ID "args")
(PUNCT "=")
(PUNCT "[")
(ID "a")
(PUNCT ".")
(ID "clone")
(PUNCT "(")
(PUNCT ")")
(PUNCT "]")
(NEWLINE)
(ID "args")
(PUNCT "[")
(LIT 0)
(PUNCT "]")
(PUNCT ".")
(ID "prefix")
(PUNCT "=")
(LIT "")
(NEWLINE)
(KEYWORD if)
(ID "b")
(KEYWORD is)
(KEYWORD not)
(KEYWORD None)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "args")
(PUNCT ".")
(ID "extend")
(PUNCT "(")
(PUNCT "[")
(ID "Comma")
(PUNCT "(")
(PUNCT ")")
(PUNCT ",")
(ID "b")
(PUNCT ".")
(ID "clone")
(PUNCT "(")
(PUNCT ")")
(PUNCT "]")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD if)
(ID "c")
(KEYWORD is)
(KEYWORD not)
(KEYWORD None)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "args")
(PUNCT ".")
(ID "extend")
(PUNCT "(")
(PUNCT "[")
(ID "Comma")
(PUNCT "(")
(PUNCT ")")
(PUNCT ",")
(ID "c")
(PUNCT ".")
(ID "clone")
(PUNCT "(")
(PUNCT ")")
(PUNCT "]")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD return)
(ID "Call")
(PUNCT "(")
(ID "Name")
(PUNCT "(")
(LIT "exec")
(PUNCT ")")
(PUNCT ",")
(ID "args")
(PUNCT ",")
(ID "prefix")
(PUNCT "=")
(ID "node")
(PUNCT ".")
(ID "prefix")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(ENDMARKER)