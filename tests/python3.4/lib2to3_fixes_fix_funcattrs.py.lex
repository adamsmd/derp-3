(LIT "Fix function attribute names (f.func_x -> f.__x__).")
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
(NEWLINE)
(KEYWORD class)
(ID "FixFuncattrs")
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
(LIT "\n    power< any+ trailer< '.' attr=('func_closure' | 'func_doc' | 'func_globals'\n                                  | 'func_name' | 'func_defaults' | 'func_code'\n                                  | 'func_dict') > any* >\n    ")
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
(ID "attr")
(PUNCT "=")
(ID "results")
(PUNCT "[")
(LIT "attr")
(PUNCT "]")
(PUNCT "[")
(LIT 0)
(PUNCT "]")
(NEWLINE)
(ID "attr")
(PUNCT ".")
(ID "replace")
(PUNCT "(")
(ID "Name")
(PUNCT "(")
(PUNCT "(")
(LIT "__%s__")
(PUNCT "%")
(ID "attr")
(PUNCT ".")
(ID "value")
(PUNCT "[")
(LIT 5)
(PUNCT ":")
(PUNCT "]")
(PUNCT ")")
(PUNCT ",")
(ID "prefix")
(PUNCT "=")
(ID "attr")
(PUNCT ".")
(ID "prefix")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(ENDMARKER)