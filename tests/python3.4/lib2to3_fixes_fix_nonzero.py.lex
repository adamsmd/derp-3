(LIT "Fixer for __nonzero__ -> __bool__ methods.")
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
(ID "Name")
(PUNCT ",")
(ID "syms")
(NEWLINE)
(KEYWORD class)
(ID "FixNonzero")
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
(LIT "\n    classdef< 'class' any+ ':'\n              suite< any*\n                     funcdef< 'def' name='__nonzero__'\n                              parameters< '(' NAME ')' > any+ >\n                     any* > >\n    ")
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
(ID "name")
(PUNCT "=")
(ID "results")
(PUNCT "[")
(LIT "name")
(PUNCT "]")
(NEWLINE)
(ID "new")
(PUNCT "=")
(ID "Name")
(PUNCT "(")
(LIT "__bool__")
(PUNCT ",")
(ID "prefix")
(PUNCT "=")
(ID "name")
(PUNCT ".")
(ID "prefix")
(PUNCT ")")
(NEWLINE)
(ID "name")
(PUNCT ".")
(ID "replace")
(PUNCT "(")
(ID "new")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(ENDMARKER)