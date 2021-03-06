(KEYWORD from)
(PUNCT ".")
(KEYWORD import)
(ID "grammar")
(PUNCT ",")
(ID "token")
(PUNCT ",")
(ID "tokenize")
(NEWLINE)
(KEYWORD class)
(ID "PgenGrammar")
(PUNCT "(")
(ID "grammar")
(PUNCT ".")
(ID "Grammar")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD pass)
(NEWLINE)
(DEDENT)
(KEYWORD class)
(ID "ParserGenerator")
(PUNCT "(")
(ID "object")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD def)
(ID "__init__")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "filename")
(PUNCT ",")
(ID "stream")
(PUNCT "=")
(KEYWORD None)
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "close_stream")
(PUNCT "=")
(KEYWORD None)
(NEWLINE)
(KEYWORD if)
(ID "stream")
(KEYWORD is)
(KEYWORD None)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "stream")
(PUNCT "=")
(ID "open")
(PUNCT "(")
(ID "filename")
(PUNCT ")")
(NEWLINE)
(ID "close_stream")
(PUNCT "=")
(ID "stream")
(PUNCT ".")
(ID "close")
(NEWLINE)
(DEDENT)
(ID "self")
(PUNCT ".")
(ID "filename")
(PUNCT "=")
(ID "filename")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "stream")
(PUNCT "=")
(ID "stream")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "generator")
(PUNCT "=")
(ID "tokenize")
(PUNCT ".")
(ID "generate_tokens")
(PUNCT "(")
(ID "stream")
(PUNCT ".")
(ID "readline")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "gettoken")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "dfas")
(PUNCT ",")
(ID "self")
(PUNCT ".")
(ID "startsymbol")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "parse")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(ID "close_stream")
(KEYWORD is)
(KEYWORD not)
(KEYWORD None)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "close_stream")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "self")
(PUNCT ".")
(ID "first")
(PUNCT "=")
(PUNCT "{")
(PUNCT "}")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "addfirstsets")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "make_grammar")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "c")
(PUNCT "=")
(ID "PgenGrammar")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "names")
(PUNCT "=")
(ID "list")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "dfas")
(PUNCT ".")
(ID "keys")
(PUNCT "(")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(ID "names")
(PUNCT ".")
(ID "sort")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "names")
(PUNCT ".")
(ID "remove")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "startsymbol")
(PUNCT ")")
(NEWLINE)
(ID "names")
(PUNCT ".")
(ID "insert")
(PUNCT "(")
(LIT 0)
(PUNCT ",")
(ID "self")
(PUNCT ".")
(ID "startsymbol")
(PUNCT ")")
(NEWLINE)
(KEYWORD for)
(ID "name")
(KEYWORD in)
(ID "names")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "i")
(PUNCT "=")
(LIT 256)
(PUNCT "+")
(ID "len")
(PUNCT "(")
(ID "c")
(PUNCT ".")
(ID "symbol2number")
(PUNCT ")")
(NEWLINE)
(ID "c")
(PUNCT ".")
(ID "symbol2number")
(PUNCT "[")
(ID "name")
(PUNCT "]")
(PUNCT "=")
(ID "i")
(NEWLINE)
(ID "c")
(PUNCT ".")
(ID "number2symbol")
(PUNCT "[")
(ID "i")
(PUNCT "]")
(PUNCT "=")
(ID "name")
(NEWLINE)
(DEDENT)
(KEYWORD for)
(ID "name")
(KEYWORD in)
(ID "names")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "dfa")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "dfas")
(PUNCT "[")
(ID "name")
(PUNCT "]")
(NEWLINE)
(ID "states")
(PUNCT "=")
(PUNCT "[")
(PUNCT "]")
(NEWLINE)
(KEYWORD for)
(ID "state")
(KEYWORD in)
(ID "dfa")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "arcs")
(PUNCT "=")
(PUNCT "[")
(PUNCT "]")
(NEWLINE)
(KEYWORD for)
(ID "label")
(PUNCT ",")
(ID "next")
(KEYWORD in)
(ID "state")
(PUNCT ".")
(ID "arcs")
(PUNCT ".")
(ID "items")
(PUNCT "(")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "arcs")
(PUNCT ".")
(ID "append")
(PUNCT "(")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "make_label")
(PUNCT "(")
(ID "c")
(PUNCT ",")
(ID "label")
(PUNCT ")")
(PUNCT ",")
(ID "dfa")
(PUNCT ".")
(ID "index")
(PUNCT "(")
(ID "next")
(PUNCT ")")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD if)
(ID "state")
(PUNCT ".")
(ID "isfinal")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "arcs")
(PUNCT ".")
(ID "append")
(PUNCT "(")
(PUNCT "(")
(LIT 0)
(PUNCT ",")
(ID "dfa")
(PUNCT ".")
(ID "index")
(PUNCT "(")
(ID "state")
(PUNCT ")")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "states")
(PUNCT ".")
(ID "append")
(PUNCT "(")
(ID "arcs")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "c")
(PUNCT ".")
(ID "states")
(PUNCT ".")
(ID "append")
(PUNCT "(")
(ID "states")
(PUNCT ")")
(NEWLINE)
(ID "c")
(PUNCT ".")
(ID "dfas")
(PUNCT "[")
(ID "c")
(PUNCT ".")
(ID "symbol2number")
(PUNCT "[")
(ID "name")
(PUNCT "]")
(PUNCT "]")
(PUNCT "=")
(PUNCT "(")
(ID "states")
(PUNCT ",")
(ID "self")
(PUNCT ".")
(ID "make_first")
(PUNCT "(")
(ID "c")
(PUNCT ",")
(ID "name")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "c")
(PUNCT ".")
(ID "start")
(PUNCT "=")
(ID "c")
(PUNCT ".")
(ID "symbol2number")
(PUNCT "[")
(ID "self")
(PUNCT ".")
(ID "startsymbol")
(PUNCT "]")
(NEWLINE)
(KEYWORD return)
(ID "c")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "make_first")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "c")
(PUNCT ",")
(ID "name")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "rawfirst")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "first")
(PUNCT "[")
(ID "name")
(PUNCT "]")
(NEWLINE)
(ID "first")
(PUNCT "=")
(PUNCT "{")
(PUNCT "}")
(NEWLINE)
(KEYWORD for)
(ID "label")
(KEYWORD in)
(ID "rawfirst")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "ilabel")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "make_label")
(PUNCT "(")
(ID "c")
(PUNCT ",")
(ID "label")
(PUNCT ")")
(NEWLINE)
(ID "first")
(PUNCT "[")
(ID "ilabel")
(PUNCT "]")
(PUNCT "=")
(LIT 1)
(NEWLINE)
(DEDENT)
(KEYWORD return)
(ID "first")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "make_label")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "c")
(PUNCT ",")
(ID "label")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "ilabel")
(PUNCT "=")
(ID "len")
(PUNCT "(")
(ID "c")
(PUNCT ".")
(ID "labels")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(ID "label")
(PUNCT "[")
(LIT 0)
(PUNCT "]")
(PUNCT ".")
(ID "isalpha")
(PUNCT "(")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "label")
(KEYWORD in)
(ID "c")
(PUNCT ".")
(ID "symbol2number")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "label")
(KEYWORD in)
(ID "c")
(PUNCT ".")
(ID "symbol2label")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(ID "c")
(PUNCT ".")
(ID "symbol2label")
(PUNCT "[")
(ID "label")
(PUNCT "]")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "c")
(PUNCT ".")
(ID "labels")
(PUNCT ".")
(ID "append")
(PUNCT "(")
(PUNCT "(")
(ID "c")
(PUNCT ".")
(ID "symbol2number")
(PUNCT "[")
(ID "label")
(PUNCT "]")
(PUNCT ",")
(KEYWORD None)
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(ID "c")
(PUNCT ".")
(ID "symbol2label")
(PUNCT "[")
(ID "label")
(PUNCT "]")
(PUNCT "=")
(ID "ilabel")
(NEWLINE)
(KEYWORD return)
(ID "ilabel")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "itoken")
(PUNCT "=")
(ID "getattr")
(PUNCT "(")
(ID "token")
(PUNCT ",")
(ID "label")
(PUNCT ",")
(KEYWORD None)
(PUNCT ")")
(NEWLINE)
(KEYWORD assert)
(ID "isinstance")
(PUNCT "(")
(ID "itoken")
(PUNCT ",")
(ID "int")
(PUNCT ")")
(PUNCT ",")
(ID "label")
(NEWLINE)
(KEYWORD assert)
(ID "itoken")
(KEYWORD in)
(ID "token")
(PUNCT ".")
(ID "tok_name")
(PUNCT ",")
(ID "label")
(NEWLINE)
(KEYWORD if)
(ID "itoken")
(KEYWORD in)
(ID "c")
(PUNCT ".")
(ID "tokens")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(ID "c")
(PUNCT ".")
(ID "tokens")
(PUNCT "[")
(ID "itoken")
(PUNCT "]")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "c")
(PUNCT ".")
(ID "labels")
(PUNCT ".")
(ID "append")
(PUNCT "(")
(PUNCT "(")
(ID "itoken")
(PUNCT ",")
(KEYWORD None)
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(ID "c")
(PUNCT ".")
(ID "tokens")
(PUNCT "[")
(ID "itoken")
(PUNCT "]")
(PUNCT "=")
(ID "ilabel")
(NEWLINE)
(KEYWORD return)
(ID "ilabel")
(NEWLINE)
(DEDENT)
(DEDENT)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD assert)
(ID "label")
(PUNCT "[")
(LIT 0)
(PUNCT "]")
(KEYWORD in)
(PUNCT "(")
(LIT "\"")
(PUNCT ",")
(LIT "'")
(PUNCT ")")
(PUNCT ",")
(ID "label")
(NEWLINE)
(ID "value")
(PUNCT "=")
(ID "eval")
(PUNCT "(")
(ID "label")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(ID "value")
(PUNCT "[")
(LIT 0)
(PUNCT "]")
(PUNCT ".")
(ID "isalpha")
(PUNCT "(")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "value")
(KEYWORD in)
(ID "c")
(PUNCT ".")
(ID "keywords")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(ID "c")
(PUNCT ".")
(ID "keywords")
(PUNCT "[")
(ID "value")
(PUNCT "]")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "c")
(PUNCT ".")
(ID "labels")
(PUNCT ".")
(ID "append")
(PUNCT "(")
(PUNCT "(")
(ID "token")
(PUNCT ".")
(ID "NAME")
(PUNCT ",")
(ID "value")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(ID "c")
(PUNCT ".")
(ID "keywords")
(PUNCT "[")
(ID "value")
(PUNCT "]")
(PUNCT "=")
(ID "ilabel")
(NEWLINE)
(KEYWORD return)
(ID "ilabel")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "itoken")
(PUNCT "=")
(ID "grammar")
(PUNCT ".")
(ID "opmap")
(PUNCT "[")
(ID "value")
(PUNCT "]")
(NEWLINE)
(KEYWORD if)
(ID "itoken")
(KEYWORD in)
(ID "c")
(PUNCT ".")
(ID "tokens")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(ID "c")
(PUNCT ".")
(ID "tokens")
(PUNCT "[")
(ID "itoken")
(PUNCT "]")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "c")
(PUNCT ".")
(ID "labels")
(PUNCT ".")
(ID "append")
(PUNCT "(")
(PUNCT "(")
(ID "itoken")
(PUNCT ",")
(KEYWORD None)
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(ID "c")
(PUNCT ".")
(ID "tokens")
(PUNCT "[")
(ID "itoken")
(PUNCT "]")
(PUNCT "=")
(ID "ilabel")
(NEWLINE)
(KEYWORD return)
(ID "ilabel")
(NEWLINE)
(DEDENT)
(DEDENT)
(DEDENT)
(DEDENT)
(KEYWORD def)
(ID "addfirstsets")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "names")
(PUNCT "=")
(ID "list")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "dfas")
(PUNCT ".")
(ID "keys")
(PUNCT "(")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(ID "names")
(PUNCT ".")
(ID "sort")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(KEYWORD for)
(ID "name")
(KEYWORD in)
(ID "names")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "name")
(KEYWORD not)
(KEYWORD in)
(ID "self")
(PUNCT ".")
(ID "first")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "calcfirst")
(PUNCT "(")
(ID "name")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(DEDENT)
(KEYWORD def)
(ID "calcfirst")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "name")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "dfa")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "dfas")
(PUNCT "[")
(ID "name")
(PUNCT "]")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "first")
(PUNCT "[")
(ID "name")
(PUNCT "]")
(PUNCT "=")
(KEYWORD None)
(NEWLINE)
(ID "state")
(PUNCT "=")
(ID "dfa")
(PUNCT "[")
(LIT 0)
(PUNCT "]")
(NEWLINE)
(ID "totalset")
(PUNCT "=")
(PUNCT "{")
(PUNCT "}")
(NEWLINE)
(ID "overlapcheck")
(PUNCT "=")
(PUNCT "{")
(PUNCT "}")
(NEWLINE)
(KEYWORD for)
(ID "label")
(PUNCT ",")
(ID "next")
(KEYWORD in)
(ID "state")
(PUNCT ".")
(ID "arcs")
(PUNCT ".")
(ID "items")
(PUNCT "(")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "label")
(KEYWORD in)
(ID "self")
(PUNCT ".")
(ID "dfas")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "label")
(KEYWORD in)
(ID "self")
(PUNCT ".")
(ID "first")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "fset")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "first")
(PUNCT "[")
(ID "label")
(PUNCT "]")
(NEWLINE)
(KEYWORD if)
(ID "fset")
(KEYWORD is)
(KEYWORD None)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD raise)
(ID "ValueError")
(PUNCT "(")
(LIT "recursion for rule %r")
(PUNCT "%")
(ID "name")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "calcfirst")
(PUNCT "(")
(ID "label")
(PUNCT ")")
(NEWLINE)
(ID "fset")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "first")
(PUNCT "[")
(ID "label")
(PUNCT "]")
(NEWLINE)
(DEDENT)
(ID "totalset")
(PUNCT ".")
(ID "update")
(PUNCT "(")
(ID "fset")
(PUNCT ")")
(NEWLINE)
(ID "overlapcheck")
(PUNCT "[")
(ID "label")
(PUNCT "]")
(PUNCT "=")
(ID "fset")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "totalset")
(PUNCT "[")
(ID "label")
(PUNCT "]")
(PUNCT "=")
(LIT 1)
(NEWLINE)
(ID "overlapcheck")
(PUNCT "[")
(ID "label")
(PUNCT "]")
(PUNCT "=")
(PUNCT "{")
(ID "label")
(PUNCT ":")
(LIT 1)
(PUNCT "}")
(NEWLINE)
(DEDENT)
(DEDENT)
(ID "inverse")
(PUNCT "=")
(PUNCT "{")
(PUNCT "}")
(NEWLINE)
(KEYWORD for)
(ID "label")
(PUNCT ",")
(ID "itsfirst")
(KEYWORD in)
(ID "overlapcheck")
(PUNCT ".")
(ID "items")
(PUNCT "(")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD for)
(ID "symbol")
(KEYWORD in)
(ID "itsfirst")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "symbol")
(KEYWORD in)
(ID "inverse")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD raise)
(ID "ValueError")
(PUNCT "(")
(LIT "rule %s is ambiguous; %s is in the")
(LIT " first sets of %s as well as %s")
(PUNCT "%")
(PUNCT "(")
(ID "name")
(PUNCT ",")
(ID "symbol")
(PUNCT ",")
(ID "label")
(PUNCT ",")
(ID "inverse")
(PUNCT "[")
(ID "symbol")
(PUNCT "]")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "inverse")
(PUNCT "[")
(ID "symbol")
(PUNCT "]")
(PUNCT "=")
(ID "label")
(NEWLINE)
(DEDENT)
(DEDENT)
(ID "self")
(PUNCT ".")
(ID "first")
(PUNCT "[")
(ID "name")
(PUNCT "]")
(PUNCT "=")
(ID "totalset")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "parse")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "dfas")
(PUNCT "=")
(PUNCT "{")
(PUNCT "}")
(NEWLINE)
(ID "startsymbol")
(PUNCT "=")
(KEYWORD None)
(NEWLINE)
(KEYWORD while)
(ID "self")
(PUNCT ".")
(ID "type")
(PUNCT "!=")
(ID "token")
(PUNCT ".")
(ID "ENDMARKER")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD while)
(ID "self")
(PUNCT ".")
(ID "type")
(PUNCT "==")
(ID "token")
(PUNCT ".")
(ID "NEWLINE")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "gettoken")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "name")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "expect")
(PUNCT "(")
(ID "token")
(PUNCT ".")
(ID "NAME")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "expect")
(PUNCT "(")
(ID "token")
(PUNCT ".")
(ID "OP")
(PUNCT ",")
(LIT ":")
(PUNCT ")")
(NEWLINE)
(ID "a")
(PUNCT ",")
(ID "z")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "parse_rhs")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "expect")
(PUNCT "(")
(ID "token")
(PUNCT ".")
(ID "NEWLINE")
(PUNCT ")")
(NEWLINE)
(ID "dfa")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "make_dfa")
(PUNCT "(")
(ID "a")
(PUNCT ",")
(ID "z")
(PUNCT ")")
(NEWLINE)
(ID "oldlen")
(PUNCT "=")
(ID "len")
(PUNCT "(")
(ID "dfa")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "simplify_dfa")
(PUNCT "(")
(ID "dfa")
(PUNCT ")")
(NEWLINE)
(ID "newlen")
(PUNCT "=")
(ID "len")
(PUNCT "(")
(ID "dfa")
(PUNCT ")")
(NEWLINE)
(ID "dfas")
(PUNCT "[")
(ID "name")
(PUNCT "]")
(PUNCT "=")
(ID "dfa")
(NEWLINE)
(KEYWORD if)
(ID "startsymbol")
(KEYWORD is)
(KEYWORD None)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "startsymbol")
(PUNCT "=")
(ID "name")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD return)
(ID "dfas")
(PUNCT ",")
(ID "startsymbol")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "make_dfa")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "start")
(PUNCT ",")
(ID "finish")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD assert)
(ID "isinstance")
(PUNCT "(")
(ID "start")
(PUNCT ",")
(ID "NFAState")
(PUNCT ")")
(NEWLINE)
(KEYWORD assert)
(ID "isinstance")
(PUNCT "(")
(ID "finish")
(PUNCT ",")
(ID "NFAState")
(PUNCT ")")
(NEWLINE)
(KEYWORD def)
(ID "closure")
(PUNCT "(")
(ID "state")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "base")
(PUNCT "=")
(PUNCT "{")
(PUNCT "}")
(NEWLINE)
(ID "addclosure")
(PUNCT "(")
(ID "state")
(PUNCT ",")
(ID "base")
(PUNCT ")")
(NEWLINE)
(KEYWORD return)
(ID "base")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "addclosure")
(PUNCT "(")
(ID "state")
(PUNCT ",")
(ID "base")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD assert)
(ID "isinstance")
(PUNCT "(")
(ID "state")
(PUNCT ",")
(ID "NFAState")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(ID "state")
(KEYWORD in)
(ID "base")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(NEWLINE)
(DEDENT)
(ID "base")
(PUNCT "[")
(ID "state")
(PUNCT "]")
(PUNCT "=")
(LIT 1)
(NEWLINE)
(KEYWORD for)
(ID "label")
(PUNCT ",")
(ID "next")
(KEYWORD in)
(ID "state")
(PUNCT ".")
(ID "arcs")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "label")
(KEYWORD is)
(KEYWORD None)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "addclosure")
(PUNCT "(")
(ID "next")
(PUNCT ",")
(ID "base")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(DEDENT)
(ID "states")
(PUNCT "=")
(PUNCT "[")
(ID "DFAState")
(PUNCT "(")
(ID "closure")
(PUNCT "(")
(ID "start")
(PUNCT ")")
(PUNCT ",")
(ID "finish")
(PUNCT ")")
(PUNCT "]")
(NEWLINE)
(KEYWORD for)
(ID "state")
(KEYWORD in)
(ID "states")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "arcs")
(PUNCT "=")
(PUNCT "{")
(PUNCT "}")
(NEWLINE)
(KEYWORD for)
(ID "nfastate")
(KEYWORD in)
(ID "state")
(PUNCT ".")
(ID "nfaset")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD for)
(ID "label")
(PUNCT ",")
(ID "next")
(KEYWORD in)
(ID "nfastate")
(PUNCT ".")
(ID "arcs")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "label")
(KEYWORD is)
(KEYWORD not)
(KEYWORD None)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "addclosure")
(PUNCT "(")
(ID "next")
(PUNCT ",")
(ID "arcs")
(PUNCT ".")
(ID "setdefault")
(PUNCT "(")
(ID "label")
(PUNCT ",")
(PUNCT "{")
(PUNCT "}")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(DEDENT)
(KEYWORD for)
(ID "label")
(PUNCT ",")
(ID "nfaset")
(KEYWORD in)
(ID "arcs")
(PUNCT ".")
(ID "items")
(PUNCT "(")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD for)
(ID "st")
(KEYWORD in)
(ID "states")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "st")
(PUNCT ".")
(ID "nfaset")
(PUNCT "==")
(ID "nfaset")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD break)
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "st")
(PUNCT "=")
(ID "DFAState")
(PUNCT "(")
(ID "nfaset")
(PUNCT ",")
(ID "finish")
(PUNCT ")")
(NEWLINE)
(ID "states")
(PUNCT ".")
(ID "append")
(PUNCT "(")
(ID "st")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "state")
(PUNCT ".")
(ID "addarc")
(PUNCT "(")
(ID "st")
(PUNCT ",")
(ID "label")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD return)
(ID "states")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "dump_nfa")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "name")
(PUNCT ",")
(ID "start")
(PUNCT ",")
(ID "finish")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "print")
(PUNCT "(")
(LIT "Dump of NFA for")
(PUNCT ",")
(ID "name")
(PUNCT ")")
(NEWLINE)
(ID "todo")
(PUNCT "=")
(PUNCT "[")
(ID "start")
(PUNCT "]")
(NEWLINE)
(KEYWORD for)
(ID "i")
(PUNCT ",")
(ID "state")
(KEYWORD in)
(ID "enumerate")
(PUNCT "(")
(ID "todo")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "print")
(PUNCT "(")
(LIT "  State")
(PUNCT ",")
(ID "i")
(PUNCT ",")
(ID "state")
(KEYWORD is)
(ID "finish")
(KEYWORD and)
(LIT "(final)")
(KEYWORD or)
(LIT "")
(PUNCT ")")
(NEWLINE)
(KEYWORD for)
(ID "label")
(PUNCT ",")
(ID "next")
(KEYWORD in)
(ID "state")
(PUNCT ".")
(ID "arcs")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "next")
(KEYWORD in)
(ID "todo")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "j")
(PUNCT "=")
(ID "todo")
(PUNCT ".")
(ID "index")
(PUNCT "(")
(ID "next")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "j")
(PUNCT "=")
(ID "len")
(PUNCT "(")
(ID "todo")
(PUNCT ")")
(NEWLINE)
(ID "todo")
(PUNCT ".")
(ID "append")
(PUNCT "(")
(ID "next")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD if)
(ID "label")
(KEYWORD is)
(KEYWORD None)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "print")
(PUNCT "(")
(LIT "    -> %d")
(PUNCT "%")
(ID "j")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "print")
(PUNCT "(")
(LIT "    %s -> %d")
(PUNCT "%")
(PUNCT "(")
(ID "label")
(PUNCT ",")
(ID "j")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(DEDENT)
(DEDENT)
(KEYWORD def)
(ID "dump_dfa")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "name")
(PUNCT ",")
(ID "dfa")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "print")
(PUNCT "(")
(LIT "Dump of DFA for")
(PUNCT ",")
(ID "name")
(PUNCT ")")
(NEWLINE)
(KEYWORD for)
(ID "i")
(PUNCT ",")
(ID "state")
(KEYWORD in)
(ID "enumerate")
(PUNCT "(")
(ID "dfa")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "print")
(PUNCT "(")
(LIT "  State")
(PUNCT ",")
(ID "i")
(PUNCT ",")
(ID "state")
(PUNCT ".")
(ID "isfinal")
(KEYWORD and)
(LIT "(final)")
(KEYWORD or)
(LIT "")
(PUNCT ")")
(NEWLINE)
(KEYWORD for)
(ID "label")
(PUNCT ",")
(ID "next")
(KEYWORD in)
(ID "state")
(PUNCT ".")
(ID "arcs")
(PUNCT ".")
(ID "items")
(PUNCT "(")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "print")
(PUNCT "(")
(LIT "    %s -> %d")
(PUNCT "%")
(PUNCT "(")
(ID "label")
(PUNCT ",")
(ID "dfa")
(PUNCT ".")
(ID "index")
(PUNCT "(")
(ID "next")
(PUNCT ")")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(DEDENT)
(KEYWORD def)
(ID "simplify_dfa")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "dfa")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "changes")
(PUNCT "=")
(KEYWORD True)
(NEWLINE)
(KEYWORD while)
(ID "changes")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "changes")
(PUNCT "=")
(KEYWORD False)
(NEWLINE)
(KEYWORD for)
(ID "i")
(PUNCT ",")
(ID "state_i")
(KEYWORD in)
(ID "enumerate")
(PUNCT "(")
(ID "dfa")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD for)
(ID "j")
(KEYWORD in)
(ID "range")
(PUNCT "(")
(ID "i")
(PUNCT "+")
(LIT 1)
(PUNCT ",")
(ID "len")
(PUNCT "(")
(ID "dfa")
(PUNCT ")")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "state_j")
(PUNCT "=")
(ID "dfa")
(PUNCT "[")
(ID "j")
(PUNCT "]")
(NEWLINE)
(KEYWORD if)
(ID "state_i")
(PUNCT "==")
(ID "state_j")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD del)
(ID "dfa")
(PUNCT "[")
(ID "j")
(PUNCT "]")
(NEWLINE)
(KEYWORD for)
(ID "state")
(KEYWORD in)
(ID "dfa")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "state")
(PUNCT ".")
(ID "unifystate")
(PUNCT "(")
(ID "state_j")
(PUNCT ",")
(ID "state_i")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "changes")
(PUNCT "=")
(KEYWORD True)
(NEWLINE)
(KEYWORD break)
(NEWLINE)
(DEDENT)
(DEDENT)
(DEDENT)
(DEDENT)
(DEDENT)
(KEYWORD def)
(ID "parse_rhs")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "a")
(PUNCT ",")
(ID "z")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "parse_alt")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(ID "self")
(PUNCT ".")
(ID "value")
(PUNCT "!=")
(LIT "|")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(ID "a")
(PUNCT ",")
(ID "z")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "aa")
(PUNCT "=")
(ID "NFAState")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "zz")
(PUNCT "=")
(ID "NFAState")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "aa")
(PUNCT ".")
(ID "addarc")
(PUNCT "(")
(ID "a")
(PUNCT ")")
(NEWLINE)
(ID "z")
(PUNCT ".")
(ID "addarc")
(PUNCT "(")
(ID "zz")
(PUNCT ")")
(NEWLINE)
(KEYWORD while)
(ID "self")
(PUNCT ".")
(ID "value")
(PUNCT "==")
(LIT "|")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "gettoken")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "a")
(PUNCT ",")
(ID "z")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "parse_alt")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "aa")
(PUNCT ".")
(ID "addarc")
(PUNCT "(")
(ID "a")
(PUNCT ")")
(NEWLINE)
(ID "z")
(PUNCT ".")
(ID "addarc")
(PUNCT "(")
(ID "zz")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD return)
(ID "aa")
(PUNCT ",")
(ID "zz")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD def)
(ID "parse_alt")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "a")
(PUNCT ",")
(ID "b")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "parse_item")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(KEYWORD while)
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "value")
(KEYWORD in)
(PUNCT "(")
(LIT "(")
(PUNCT ",")
(LIT "[")
(PUNCT ")")
(KEYWORD or)
(ID "self")
(PUNCT ".")
(ID "type")
(KEYWORD in)
(PUNCT "(")
(ID "token")
(PUNCT ".")
(ID "NAME")
(PUNCT ",")
(ID "token")
(PUNCT ".")
(ID "STRING")
(PUNCT ")")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "c")
(PUNCT ",")
(ID "d")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "parse_item")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "b")
(PUNCT ".")
(ID "addarc")
(PUNCT "(")
(ID "c")
(PUNCT ")")
(NEWLINE)
(ID "b")
(PUNCT "=")
(ID "d")
(NEWLINE)
(DEDENT)
(KEYWORD return)
(ID "a")
(PUNCT ",")
(ID "b")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "parse_item")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "self")
(PUNCT ".")
(ID "value")
(PUNCT "==")
(LIT "[")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "gettoken")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "a")
(PUNCT ",")
(ID "z")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "parse_rhs")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "expect")
(PUNCT "(")
(ID "token")
(PUNCT ".")
(ID "OP")
(PUNCT ",")
(LIT "]")
(PUNCT ")")
(NEWLINE)
(ID "a")
(PUNCT ".")
(ID "addarc")
(PUNCT "(")
(ID "z")
(PUNCT ")")
(NEWLINE)
(KEYWORD return)
(ID "a")
(PUNCT ",")
(ID "z")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "a")
(PUNCT ",")
(ID "z")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "parse_atom")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "value")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "value")
(NEWLINE)
(KEYWORD if)
(ID "value")
(KEYWORD not)
(KEYWORD in)
(PUNCT "(")
(LIT "+")
(PUNCT ",")
(LIT "*")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(ID "a")
(PUNCT ",")
(ID "z")
(NEWLINE)
(DEDENT)
(ID "self")
(PUNCT ".")
(ID "gettoken")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "z")
(PUNCT ".")
(ID "addarc")
(PUNCT "(")
(ID "a")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(ID "value")
(PUNCT "==")
(LIT "+")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(ID "a")
(PUNCT ",")
(ID "z")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(ID "a")
(PUNCT ",")
(ID "a")
(NEWLINE)
(DEDENT)
(DEDENT)
(DEDENT)
(KEYWORD def)
(ID "parse_atom")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "self")
(PUNCT ".")
(ID "value")
(PUNCT "==")
(LIT "(")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "gettoken")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "a")
(PUNCT ",")
(ID "z")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "parse_rhs")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "expect")
(PUNCT "(")
(ID "token")
(PUNCT ".")
(ID "OP")
(PUNCT ",")
(LIT ")")
(PUNCT ")")
(NEWLINE)
(KEYWORD return)
(ID "a")
(PUNCT ",")
(ID "z")
(NEWLINE)
(DEDENT)
(KEYWORD elif)
(ID "self")
(PUNCT ".")
(ID "type")
(KEYWORD in)
(PUNCT "(")
(ID "token")
(PUNCT ".")
(ID "NAME")
(PUNCT ",")
(ID "token")
(PUNCT ".")
(ID "STRING")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "a")
(PUNCT "=")
(ID "NFAState")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "z")
(PUNCT "=")
(ID "NFAState")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "a")
(PUNCT ".")
(ID "addarc")
(PUNCT "(")
(ID "z")
(PUNCT ",")
(ID "self")
(PUNCT ".")
(ID "value")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "gettoken")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(KEYWORD return)
(ID "a")
(PUNCT ",")
(ID "z")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "raise_error")
(PUNCT "(")
(LIT "expected (...) or NAME or STRING, got %s/%s")
(PUNCT ",")
(ID "self")
(PUNCT ".")
(ID "type")
(PUNCT ",")
(ID "self")
(PUNCT ".")
(ID "value")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD def)
(ID "expect")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "type")
(PUNCT ",")
(ID "value")
(PUNCT "=")
(KEYWORD None)
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "self")
(PUNCT ".")
(ID "type")
(PUNCT "!=")
(ID "type")
(KEYWORD or)
(PUNCT "(")
(ID "value")
(KEYWORD is)
(KEYWORD not)
(KEYWORD None)
(KEYWORD and)
(ID "self")
(PUNCT ".")
(ID "value")
(PUNCT "!=")
(ID "value")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "raise_error")
(PUNCT "(")
(LIT "expected %s/%s, got %s/%s")
(PUNCT ",")
(ID "type")
(PUNCT ",")
(ID "value")
(PUNCT ",")
(ID "self")
(PUNCT ".")
(ID "type")
(PUNCT ",")
(ID "self")
(PUNCT ".")
(ID "value")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "value")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "value")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "gettoken")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(KEYWORD return)
(ID "value")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "gettoken")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "tup")
(PUNCT "=")
(ID "next")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "generator")
(PUNCT ")")
(NEWLINE)
(KEYWORD while)
(ID "tup")
(PUNCT "[")
(LIT 0)
(PUNCT "]")
(KEYWORD in)
(PUNCT "(")
(ID "tokenize")
(PUNCT ".")
(ID "COMMENT")
(PUNCT ",")
(ID "tokenize")
(PUNCT ".")
(ID "NL")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "tup")
(PUNCT "=")
(ID "next")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "generator")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "self")
(PUNCT ".")
(ID "type")
(PUNCT ",")
(ID "self")
(PUNCT ".")
(ID "value")
(PUNCT ",")
(ID "self")
(PUNCT ".")
(ID "begin")
(PUNCT ",")
(ID "self")
(PUNCT ".")
(ID "end")
(PUNCT ",")
(ID "self")
(PUNCT ".")
(ID "line")
(PUNCT "=")
(ID "tup")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "raise_error")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "msg")
(PUNCT ",")
(PUNCT "*")
(ID "args")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "args")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD try)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "msg")
(PUNCT "=")
(ID "msg")
(PUNCT "%")
(ID "args")
(NEWLINE)
(DEDENT)
(KEYWORD except)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "msg")
(PUNCT "=")
(LIT " ")
(PUNCT ".")
(ID "join")
(PUNCT "(")
(PUNCT "[")
(ID "msg")
(PUNCT "]")
(PUNCT "+")
(ID "list")
(PUNCT "(")
(ID "map")
(PUNCT "(")
(ID "str")
(PUNCT ",")
(ID "args")
(PUNCT ")")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD raise)
(ID "SyntaxError")
(PUNCT "(")
(ID "msg")
(PUNCT ",")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "filename")
(PUNCT ",")
(ID "self")
(PUNCT ".")
(ID "end")
(PUNCT "[")
(LIT 0)
(PUNCT "]")
(PUNCT ",")
(ID "self")
(PUNCT ".")
(ID "end")
(PUNCT "[")
(LIT 1)
(PUNCT "]")
(PUNCT ",")
(ID "self")
(PUNCT ".")
(ID "line")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD class)
(ID "NFAState")
(PUNCT "(")
(ID "object")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD def)
(ID "__init__")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "arcs")
(PUNCT "=")
(PUNCT "[")
(PUNCT "]")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "addarc")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "next")
(PUNCT ",")
(ID "label")
(PUNCT "=")
(KEYWORD None)
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD assert)
(ID "label")
(KEYWORD is)
(KEYWORD None)
(KEYWORD or)
(ID "isinstance")
(PUNCT "(")
(ID "label")
(PUNCT ",")
(ID "str")
(PUNCT ")")
(NEWLINE)
(KEYWORD assert)
(ID "isinstance")
(PUNCT "(")
(ID "next")
(PUNCT ",")
(ID "NFAState")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "arcs")
(PUNCT ".")
(ID "append")
(PUNCT "(")
(PUNCT "(")
(ID "label")
(PUNCT ",")
(ID "next")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD class)
(ID "DFAState")
(PUNCT "(")
(ID "object")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD def)
(ID "__init__")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "nfaset")
(PUNCT ",")
(ID "final")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD assert)
(ID "isinstance")
(PUNCT "(")
(ID "nfaset")
(PUNCT ",")
(ID "dict")
(PUNCT ")")
(NEWLINE)
(KEYWORD assert)
(ID "isinstance")
(PUNCT "(")
(ID "next")
(PUNCT "(")
(ID "iter")
(PUNCT "(")
(ID "nfaset")
(PUNCT ")")
(PUNCT ")")
(PUNCT ",")
(ID "NFAState")
(PUNCT ")")
(NEWLINE)
(KEYWORD assert)
(ID "isinstance")
(PUNCT "(")
(ID "final")
(PUNCT ",")
(ID "NFAState")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "nfaset")
(PUNCT "=")
(ID "nfaset")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "isfinal")
(PUNCT "=")
(ID "final")
(KEYWORD in)
(ID "nfaset")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "arcs")
(PUNCT "=")
(PUNCT "{")
(PUNCT "}")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "addarc")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "next")
(PUNCT ",")
(ID "label")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD assert)
(ID "isinstance")
(PUNCT "(")
(ID "label")
(PUNCT ",")
(ID "str")
(PUNCT ")")
(NEWLINE)
(KEYWORD assert)
(ID "label")
(KEYWORD not)
(KEYWORD in)
(ID "self")
(PUNCT ".")
(ID "arcs")
(NEWLINE)
(KEYWORD assert)
(ID "isinstance")
(PUNCT "(")
(ID "next")
(PUNCT ",")
(ID "DFAState")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "arcs")
(PUNCT "[")
(ID "label")
(PUNCT "]")
(PUNCT "=")
(ID "next")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "unifystate")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "old")
(PUNCT ",")
(ID "new")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD for)
(ID "label")
(PUNCT ",")
(ID "next")
(KEYWORD in)
(ID "self")
(PUNCT ".")
(ID "arcs")
(PUNCT ".")
(ID "items")
(PUNCT "(")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "next")
(KEYWORD is)
(ID "old")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "arcs")
(PUNCT "[")
(ID "label")
(PUNCT "]")
(PUNCT "=")
(ID "new")
(NEWLINE)
(DEDENT)
(DEDENT)
(DEDENT)
(KEYWORD def)
(ID "__eq__")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "other")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD assert)
(ID "isinstance")
(PUNCT "(")
(ID "other")
(PUNCT ",")
(ID "DFAState")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(ID "self")
(PUNCT ".")
(ID "isfinal")
(PUNCT "!=")
(ID "other")
(PUNCT ".")
(ID "isfinal")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(KEYWORD False)
(NEWLINE)
(DEDENT)
(KEYWORD if)
(ID "len")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "arcs")
(PUNCT ")")
(PUNCT "!=")
(ID "len")
(PUNCT "(")
(ID "other")
(PUNCT ".")
(ID "arcs")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(KEYWORD False)
(NEWLINE)
(DEDENT)
(KEYWORD for)
(ID "label")
(PUNCT ",")
(ID "next")
(KEYWORD in)
(ID "self")
(PUNCT ".")
(ID "arcs")
(PUNCT ".")
(ID "items")
(PUNCT "(")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "next")
(KEYWORD is)
(KEYWORD not)
(ID "other")
(PUNCT ".")
(ID "arcs")
(PUNCT ".")
(ID "get")
(PUNCT "(")
(ID "label")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(KEYWORD False)
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD return)
(KEYWORD True)
(NEWLINE)
(DEDENT)
(ID "__hash__")
(PUNCT "=")
(KEYWORD None)
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "generate_grammar")
(PUNCT "(")
(ID "filename")
(PUNCT "=")
(LIT "Grammar.txt")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "p")
(PUNCT "=")
(ID "ParserGenerator")
(PUNCT "(")
(ID "filename")
(PUNCT ")")
(NEWLINE)
(KEYWORD return)
(ID "p")
(PUNCT ".")
(ID "make_grammar")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ENDMARKER)
