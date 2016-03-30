(LIT "Fixer that cleans up a tuple argument to isinstance after the tokens\nin it were fixed.  This is mainly used to remove double occurrences of\ntokens as a leftover of the long -> int / unicode -> str conversion.\n\neg.  isinstance(x, (int, long)) -> isinstance(x, (int, int))\n       -> isinstance(x, int)\n")
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
(ID "token")
(NEWLINE)
(KEYWORD class)
(ID "FixIsinstance")
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
(LIT "\n    power<\n        'isinstance'\n        trailer< '(' arglist< any ',' atom< '('\n            args=testlist_gexp< any+ >\n        ')' > > ')' >\n    >\n    ")
(NEWLINE)
(ID "run_order")
(PUNCT "=")
(LIT 6)
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
(ID "names_inserted")
(PUNCT "=")
(ID "set")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "testlist")
(PUNCT "=")
(ID "results")
(PUNCT "[")
(LIT "args")
(PUNCT "]")
(NEWLINE)
(ID "args")
(PUNCT "=")
(ID "testlist")
(PUNCT ".")
(ID "children")
(NEWLINE)
(ID "new_args")
(PUNCT "=")
(PUNCT "[")
(PUNCT "]")
(NEWLINE)
(ID "iterator")
(PUNCT "=")
(ID "enumerate")
(PUNCT "(")
(ID "args")
(PUNCT ")")
(NEWLINE)
(KEYWORD for)
(ID "idx")
(PUNCT ",")
(ID "arg")
(KEYWORD in)
(ID "iterator")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "arg")
(PUNCT ".")
(ID "type")
(PUNCT "==")
(ID "token")
(PUNCT ".")
(ID "NAME")
(KEYWORD and)
(ID "arg")
(PUNCT ".")
(ID "value")
(KEYWORD in)
(ID "names_inserted")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "idx")
(PUNCT "<")
(ID "len")
(PUNCT "(")
(ID "args")
(PUNCT ")")
(PUNCT "-")
(LIT 1)
(KEYWORD and)
(ID "args")
(PUNCT "[")
(ID "idx")
(PUNCT "+")
(LIT 1)
(PUNCT "]")
(PUNCT ".")
(ID "type")
(PUNCT "==")
(ID "token")
(PUNCT ".")
(ID "COMMA")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "next")
(PUNCT "(")
(ID "iterator")
(PUNCT ")")
(NEWLINE)
(KEYWORD continue)
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "new_args")
(PUNCT ".")
(ID "append")
(PUNCT "(")
(ID "arg")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(ID "arg")
(PUNCT ".")
(ID "type")
(PUNCT "==")
(ID "token")
(PUNCT ".")
(ID "NAME")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "names_inserted")
(PUNCT ".")
(ID "add")
(PUNCT "(")
(ID "arg")
(PUNCT ".")
(ID "value")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(DEDENT)
(KEYWORD if)
(ID "new_args")
(KEYWORD and)
(ID "new_args")
(PUNCT "[")
(PUNCT "-")
(LIT 1)
(PUNCT "]")
(PUNCT ".")
(ID "type")
(PUNCT "==")
(ID "token")
(PUNCT ".")
(ID "COMMA")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD del)
(ID "new_args")
(PUNCT "[")
(PUNCT "-")
(LIT 1)
(PUNCT "]")
(NEWLINE)
(DEDENT)
(KEYWORD if)
(ID "len")
(PUNCT "(")
(ID "new_args")
(PUNCT ")")
(PUNCT "==")
(LIT 1)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "atom")
(PUNCT "=")
(ID "testlist")
(PUNCT ".")
(ID "parent")
(NEWLINE)
(ID "new_args")
(PUNCT "[")
(LIT 0)
(PUNCT "]")
(PUNCT ".")
(ID "prefix")
(PUNCT "=")
(ID "atom")
(PUNCT ".")
(ID "prefix")
(NEWLINE)
(ID "atom")
(PUNCT ".")
(ID "replace")
(PUNCT "(")
(ID "new_args")
(PUNCT "[")
(LIT 0)
(PUNCT "]")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "args")
(PUNCT "[")
(PUNCT ":")
(PUNCT "]")
(PUNCT "=")
(ID "new_args")
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
