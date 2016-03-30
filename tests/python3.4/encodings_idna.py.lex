(KEYWORD import)
(ID "stringprep")
(PUNCT ",")
(ID "re")
(PUNCT ",")
(ID "codecs")
(NEWLINE)
(KEYWORD from)
(ID "unicodedata")
(KEYWORD import)
(ID "ucd_3_2_0")
(KEYWORD as)
(ID "unicodedata")
(NEWLINE)
(ID "dots")
(PUNCT "=")
(ID "re")
(PUNCT ".")
(ID "compile")
(PUNCT "(")
(LIT "[.。．｡]")
(PUNCT ")")
(NEWLINE)
(ID "ace_prefix")
(PUNCT "=")
(LIT #"xn--")
(NEWLINE)
(ID "sace_prefix")
(PUNCT "=")
(LIT "xn--")
(NEWLINE)
(KEYWORD def)
(ID "nameprep")
(PUNCT "(")
(ID "label")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "newlabel")
(PUNCT "=")
(PUNCT "[")
(PUNCT "]")
(NEWLINE)
(KEYWORD for)
(ID "c")
(KEYWORD in)
(ID "label")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "stringprep")
(PUNCT ".")
(ID "in_table_b1")
(PUNCT "(")
(ID "c")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD continue)
(NEWLINE)
(DEDENT)
(ID "newlabel")
(PUNCT ".")
(ID "append")
(PUNCT "(")
(ID "stringprep")
(PUNCT ".")
(ID "map_table_b2")
(PUNCT "(")
(ID "c")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "label")
(PUNCT "=")
(LIT "")
(PUNCT ".")
(ID "join")
(PUNCT "(")
(ID "newlabel")
(PUNCT ")")
(NEWLINE)
(ID "label")
(PUNCT "=")
(ID "unicodedata")
(PUNCT ".")
(ID "normalize")
(PUNCT "(")
(LIT "NFKC")
(PUNCT ",")
(ID "label")
(PUNCT ")")
(NEWLINE)
(KEYWORD for)
(ID "c")
(KEYWORD in)
(ID "label")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "stringprep")
(PUNCT ".")
(ID "in_table_c12")
(PUNCT "(")
(ID "c")
(PUNCT ")")
(KEYWORD or)
(ID "stringprep")
(PUNCT ".")
(ID "in_table_c22")
(PUNCT "(")
(ID "c")
(PUNCT ")")
(KEYWORD or)
(ID "stringprep")
(PUNCT ".")
(ID "in_table_c3")
(PUNCT "(")
(ID "c")
(PUNCT ")")
(KEYWORD or)
(ID "stringprep")
(PUNCT ".")
(ID "in_table_c4")
(PUNCT "(")
(ID "c")
(PUNCT ")")
(KEYWORD or)
(ID "stringprep")
(PUNCT ".")
(ID "in_table_c5")
(PUNCT "(")
(ID "c")
(PUNCT ")")
(KEYWORD or)
(ID "stringprep")
(PUNCT ".")
(ID "in_table_c6")
(PUNCT "(")
(ID "c")
(PUNCT ")")
(KEYWORD or)
(ID "stringprep")
(PUNCT ".")
(ID "in_table_c7")
(PUNCT "(")
(ID "c")
(PUNCT ")")
(KEYWORD or)
(ID "stringprep")
(PUNCT ".")
(ID "in_table_c8")
(PUNCT "(")
(ID "c")
(PUNCT ")")
(KEYWORD or)
(ID "stringprep")
(PUNCT ".")
(ID "in_table_c9")
(PUNCT "(")
(ID "c")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD raise)
(ID "UnicodeError")
(PUNCT "(")
(LIT "Invalid character %r")
(PUNCT "%")
(ID "c")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(ID "RandAL")
(PUNCT "=")
(PUNCT "[")
(ID "stringprep")
(PUNCT ".")
(ID "in_table_d1")
(PUNCT "(")
(ID "x")
(PUNCT ")")
(KEYWORD for)
(ID "x")
(KEYWORD in)
(ID "label")
(PUNCT "]")
(NEWLINE)
(KEYWORD for)
(ID "c")
(KEYWORD in)
(ID "RandAL")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "c")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "any")
(PUNCT "(")
(ID "stringprep")
(PUNCT ".")
(ID "in_table_d2")
(PUNCT "(")
(ID "x")
(PUNCT ")")
(KEYWORD for)
(ID "x")
(KEYWORD in)
(ID "label")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD raise)
(ID "UnicodeError")
(PUNCT "(")
(LIT "Violation of BIDI requirement 2")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD if)
(KEYWORD not)
(ID "RandAL")
(PUNCT "[")
(LIT 0)
(PUNCT "]")
(KEYWORD or)
(KEYWORD not)
(ID "RandAL")
(PUNCT "[")
(PUNCT "-")
(LIT 1)
(PUNCT "]")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD raise)
(ID "UnicodeError")
(PUNCT "(")
(LIT "Violation of BIDI requirement 3")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(DEDENT)
(KEYWORD return)
(ID "label")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "ToASCII")
(PUNCT "(")
(ID "label")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD try)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "label")
(PUNCT "=")
(ID "label")
(PUNCT ".")
(ID "encode")
(PUNCT "(")
(LIT "ascii")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD except)
(ID "UnicodeError")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD pass)
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(LIT 0)
(PUNCT "<")
(ID "len")
(PUNCT "(")
(ID "label")
(PUNCT ")")
(PUNCT "<")
(LIT 64)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(ID "label")
(NEWLINE)
(DEDENT)
(KEYWORD raise)
(ID "UnicodeError")
(PUNCT "(")
(LIT "label empty or too long")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "label")
(PUNCT "=")
(ID "nameprep")
(PUNCT "(")
(ID "label")
(PUNCT ")")
(NEWLINE)
(KEYWORD try)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "label")
(PUNCT "=")
(ID "label")
(PUNCT ".")
(ID "encode")
(PUNCT "(")
(LIT "ascii")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD except)
(ID "UnicodeError")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD pass)
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(LIT 0)
(PUNCT "<")
(ID "len")
(PUNCT "(")
(ID "label")
(PUNCT ")")
(PUNCT "<")
(LIT 64)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(ID "label")
(NEWLINE)
(DEDENT)
(KEYWORD raise)
(ID "UnicodeError")
(PUNCT "(")
(LIT "label empty or too long")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD if)
(ID "label")
(PUNCT ".")
(ID "startswith")
(PUNCT "(")
(ID "sace_prefix")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD raise)
(ID "UnicodeError")
(PUNCT "(")
(LIT "Label starts with ACE prefix")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "label")
(PUNCT "=")
(ID "label")
(PUNCT ".")
(ID "encode")
(PUNCT "(")
(LIT "punycode")
(PUNCT ")")
(NEWLINE)
(ID "label")
(PUNCT "=")
(ID "ace_prefix")
(PUNCT "+")
(ID "label")
(NEWLINE)
(KEYWORD if)
(LIT 0)
(PUNCT "<")
(ID "len")
(PUNCT "(")
(ID "label")
(PUNCT ")")
(PUNCT "<")
(LIT 64)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(ID "label")
(NEWLINE)
(DEDENT)
(KEYWORD raise)
(ID "UnicodeError")
(PUNCT "(")
(LIT "label empty or too long")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "ToUnicode")
(PUNCT "(")
(ID "label")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "isinstance")
(PUNCT "(")
(ID "label")
(PUNCT ",")
(ID "bytes")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "pure_ascii")
(PUNCT "=")
(KEYWORD True)
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD try)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "label")
(PUNCT "=")
(ID "label")
(PUNCT ".")
(ID "encode")
(PUNCT "(")
(LIT "ascii")
(PUNCT ")")
(NEWLINE)
(ID "pure_ascii")
(PUNCT "=")
(KEYWORD True)
(NEWLINE)
(DEDENT)
(KEYWORD except)
(ID "UnicodeError")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "pure_ascii")
(PUNCT "=")
(KEYWORD False)
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD if)
(KEYWORD not)
(ID "pure_ascii")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "label")
(PUNCT "=")
(ID "nameprep")
(PUNCT "(")
(ID "label")
(PUNCT ")")
(NEWLINE)
(KEYWORD try)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "label")
(PUNCT "=")
(ID "label")
(PUNCT ".")
(ID "encode")
(PUNCT "(")
(LIT "ascii")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD except)
(ID "UnicodeError")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD raise)
(ID "UnicodeError")
(PUNCT "(")
(LIT "Invalid character in IDN label")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD if)
(KEYWORD not)
(ID "label")
(PUNCT ".")
(ID "startswith")
(PUNCT "(")
(ID "ace_prefix")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(ID "str")
(PUNCT "(")
(ID "label")
(PUNCT ",")
(LIT "ascii")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "label1")
(PUNCT "=")
(ID "label")
(PUNCT "[")
(ID "len")
(PUNCT "(")
(ID "ace_prefix")
(PUNCT ")")
(PUNCT ":")
(PUNCT "]")
(NEWLINE)
(ID "result")
(PUNCT "=")
(ID "label1")
(PUNCT ".")
(ID "decode")
(PUNCT "(")
(LIT "punycode")
(PUNCT ")")
(NEWLINE)
(ID "label2")
(PUNCT "=")
(ID "ToASCII")
(PUNCT "(")
(ID "result")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(ID "str")
(PUNCT "(")
(ID "label")
(PUNCT ",")
(LIT "ascii")
(PUNCT ")")
(PUNCT ".")
(ID "lower")
(PUNCT "(")
(PUNCT ")")
(PUNCT "!=")
(ID "str")
(PUNCT "(")
(ID "label2")
(PUNCT ",")
(LIT "ascii")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD raise)
(ID "UnicodeError")
(PUNCT "(")
(LIT "IDNA does not round-trip")
(PUNCT ",")
(ID "label")
(PUNCT ",")
(ID "label2")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD return)
(ID "result")
(NEWLINE)
(DEDENT)
(KEYWORD class)
(ID "Codec")
(PUNCT "(")
(ID "codecs")
(PUNCT ".")
(ID "Codec")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD def)
(ID "encode")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "input")
(PUNCT ",")
(ID "errors")
(PUNCT "=")
(LIT "strict")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "errors")
(PUNCT "!=")
(LIT "strict")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD raise)
(ID "UnicodeError")
(PUNCT "(")
(LIT "unsupported error handling ")
(PUNCT "+")
(ID "errors")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD if)
(KEYWORD not)
(ID "input")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(LIT #"")
(PUNCT ",")
(LIT 0)
(NEWLINE)
(DEDENT)
(KEYWORD try)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "result")
(PUNCT "=")
(ID "input")
(PUNCT ".")
(ID "encode")
(PUNCT "(")
(LIT "ascii")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD except)
(ID "UnicodeEncodeError")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD pass)
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "labels")
(PUNCT "=")
(ID "result")
(PUNCT ".")
(ID "split")
(PUNCT "(")
(LIT #".")
(PUNCT ")")
(NEWLINE)
(KEYWORD for)
(ID "label")
(KEYWORD in)
(ID "labels")
(PUNCT "[")
(PUNCT ":")
(PUNCT "-")
(LIT 1)
(PUNCT "]")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(KEYWORD not)
(PUNCT "(")
(LIT 0)
(PUNCT "<")
(ID "len")
(PUNCT "(")
(ID "label")
(PUNCT ")")
(PUNCT "<")
(LIT 64)
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD raise)
(ID "UnicodeError")
(PUNCT "(")
(LIT "label empty or too long")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD if)
(ID "len")
(PUNCT "(")
(ID "labels")
(PUNCT "[")
(PUNCT "-")
(LIT 1)
(PUNCT "]")
(PUNCT ")")
(PUNCT ">=")
(LIT 64)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD raise)
(ID "UnicodeError")
(PUNCT "(")
(LIT "label too long")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD return)
(ID "result")
(PUNCT ",")
(ID "len")
(PUNCT "(")
(ID "input")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "result")
(PUNCT "=")
(ID "bytearray")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "labels")
(PUNCT "=")
(ID "dots")
(PUNCT ".")
(ID "split")
(PUNCT "(")
(ID "input")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(ID "labels")
(KEYWORD and)
(KEYWORD not)
(ID "labels")
(PUNCT "[")
(PUNCT "-")
(LIT 1)
(PUNCT "]")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "trailing_dot")
(PUNCT "=")
(LIT #".")
(NEWLINE)
(KEYWORD del)
(ID "labels")
(PUNCT "[")
(PUNCT "-")
(LIT 1)
(PUNCT "]")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "trailing_dot")
(PUNCT "=")
(LIT #"")
(NEWLINE)
(DEDENT)
(KEYWORD for)
(ID "label")
(KEYWORD in)
(ID "labels")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "result")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "result")
(PUNCT ".")
(ID "extend")
(PUNCT "(")
(LIT #".")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "result")
(PUNCT ".")
(ID "extend")
(PUNCT "(")
(ID "ToASCII")
(PUNCT "(")
(ID "label")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD return)
(ID "bytes")
(PUNCT "(")
(ID "result")
(PUNCT "+")
(ID "trailing_dot")
(PUNCT ")")
(PUNCT ",")
(ID "len")
(PUNCT "(")
(ID "input")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "decode")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "input")
(PUNCT ",")
(ID "errors")
(PUNCT "=")
(LIT "strict")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "errors")
(PUNCT "!=")
(LIT "strict")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD raise)
(ID "UnicodeError")
(PUNCT "(")
(LIT "Unsupported error handling ")
(PUNCT "+")
(ID "errors")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD if)
(KEYWORD not)
(ID "input")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(LIT "")
(PUNCT ",")
(LIT 0)
(NEWLINE)
(DEDENT)
(KEYWORD if)
(KEYWORD not)
(ID "isinstance")
(PUNCT "(")
(ID "input")
(PUNCT ",")
(ID "bytes")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "input")
(PUNCT "=")
(ID "bytes")
(PUNCT "(")
(ID "input")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD if)
(ID "ace_prefix")
(KEYWORD not)
(KEYWORD in)
(ID "input")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD try)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(ID "input")
(PUNCT ".")
(ID "decode")
(PUNCT "(")
(LIT "ascii")
(PUNCT ")")
(PUNCT ",")
(ID "len")
(PUNCT "(")
(ID "input")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD except)
(ID "UnicodeDecodeError")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD pass)
(NEWLINE)
(DEDENT)
(DEDENT)
(ID "labels")
(PUNCT "=")
(ID "input")
(PUNCT ".")
(ID "split")
(PUNCT "(")
(LIT #".")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(ID "labels")
(KEYWORD and)
(ID "len")
(PUNCT "(")
(ID "labels")
(PUNCT "[")
(PUNCT "-")
(LIT 1)
(PUNCT "]")
(PUNCT ")")
(PUNCT "==")
(LIT 0)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "trailing_dot")
(PUNCT "=")
(LIT ".")
(NEWLINE)
(KEYWORD del)
(ID "labels")
(PUNCT "[")
(PUNCT "-")
(LIT 1)
(PUNCT "]")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "trailing_dot")
(PUNCT "=")
(LIT "")
(NEWLINE)
(DEDENT)
(ID "result")
(PUNCT "=")
(PUNCT "[")
(PUNCT "]")
(NEWLINE)
(KEYWORD for)
(ID "label")
(KEYWORD in)
(ID "labels")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "result")
(PUNCT ".")
(ID "append")
(PUNCT "(")
(ID "ToUnicode")
(PUNCT "(")
(ID "label")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD return)
(LIT ".")
(PUNCT ".")
(ID "join")
(PUNCT "(")
(ID "result")
(PUNCT ")")
(PUNCT "+")
(ID "trailing_dot")
(PUNCT ",")
(ID "len")
(PUNCT "(")
(ID "input")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD class)
(ID "IncrementalEncoder")
(PUNCT "(")
(ID "codecs")
(PUNCT ".")
(ID "BufferedIncrementalEncoder")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD def)
(ID "_buffer_encode")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "input")
(PUNCT ",")
(ID "errors")
(PUNCT ",")
(ID "final")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "errors")
(PUNCT "!=")
(LIT "strict")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD raise)
(ID "UnicodeError")
(PUNCT "(")
(LIT "unsupported error handling ")
(PUNCT "+")
(ID "errors")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD if)
(KEYWORD not)
(ID "input")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(PUNCT "(")
(LIT #"")
(PUNCT ",")
(LIT 0)
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "labels")
(PUNCT "=")
(ID "dots")
(PUNCT ".")
(ID "split")
(PUNCT "(")
(ID "input")
(PUNCT ")")
(NEWLINE)
(ID "trailing_dot")
(PUNCT "=")
(LIT #"")
(NEWLINE)
(KEYWORD if)
(ID "labels")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(KEYWORD not)
(ID "labels")
(PUNCT "[")
(PUNCT "-")
(LIT 1)
(PUNCT "]")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "trailing_dot")
(PUNCT "=")
(LIT #".")
(NEWLINE)
(KEYWORD del)
(ID "labels")
(PUNCT "[")
(PUNCT "-")
(LIT 1)
(PUNCT "]")
(NEWLINE)
(DEDENT)
(KEYWORD elif)
(KEYWORD not)
(ID "final")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD del)
(ID "labels")
(PUNCT "[")
(PUNCT "-")
(LIT 1)
(PUNCT "]")
(NEWLINE)
(KEYWORD if)
(ID "labels")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "trailing_dot")
(PUNCT "=")
(LIT #".")
(NEWLINE)
(DEDENT)
(DEDENT)
(DEDENT)
(ID "result")
(PUNCT "=")
(ID "bytearray")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "size")
(PUNCT "=")
(LIT 0)
(NEWLINE)
(KEYWORD for)
(ID "label")
(KEYWORD in)
(ID "labels")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "size")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "result")
(PUNCT ".")
(ID "extend")
(PUNCT "(")
(LIT #".")
(PUNCT ")")
(NEWLINE)
(ID "size")
(PUNCT "+=")
(LIT 1)
(NEWLINE)
(DEDENT)
(ID "result")
(PUNCT ".")
(ID "extend")
(PUNCT "(")
(ID "ToASCII")
(PUNCT "(")
(ID "label")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(ID "size")
(PUNCT "+=")
(ID "len")
(PUNCT "(")
(ID "label")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "result")
(PUNCT "+=")
(ID "trailing_dot")
(NEWLINE)
(ID "size")
(PUNCT "+=")
(ID "len")
(PUNCT "(")
(ID "trailing_dot")
(PUNCT ")")
(NEWLINE)
(KEYWORD return)
(PUNCT "(")
(ID "bytes")
(PUNCT "(")
(ID "result")
(PUNCT ")")
(PUNCT ",")
(ID "size")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD class)
(ID "IncrementalDecoder")
(PUNCT "(")
(ID "codecs")
(PUNCT ".")
(ID "BufferedIncrementalDecoder")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD def)
(ID "_buffer_decode")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "input")
(PUNCT ",")
(ID "errors")
(PUNCT ",")
(ID "final")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "errors")
(PUNCT "!=")
(LIT "strict")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD raise)
(ID "UnicodeError")
(PUNCT "(")
(LIT "Unsupported error handling ")
(PUNCT "+")
(ID "errors")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD if)
(KEYWORD not)
(ID "input")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(PUNCT "(")
(LIT "")
(PUNCT ",")
(LIT 0)
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD if)
(ID "isinstance")
(PUNCT "(")
(ID "input")
(PUNCT ",")
(ID "str")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "labels")
(PUNCT "=")
(ID "dots")
(PUNCT ".")
(ID "split")
(PUNCT "(")
(ID "input")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "input")
(PUNCT "=")
(ID "str")
(PUNCT "(")
(ID "input")
(PUNCT ",")
(LIT "ascii")
(PUNCT ")")
(NEWLINE)
(ID "labels")
(PUNCT "=")
(ID "input")
(PUNCT ".")
(ID "split")
(PUNCT "(")
(LIT ".")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "trailing_dot")
(PUNCT "=")
(LIT "")
(NEWLINE)
(KEYWORD if)
(ID "labels")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(KEYWORD not)
(ID "labels")
(PUNCT "[")
(PUNCT "-")
(LIT 1)
(PUNCT "]")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "trailing_dot")
(PUNCT "=")
(LIT ".")
(NEWLINE)
(KEYWORD del)
(ID "labels")
(PUNCT "[")
(PUNCT "-")
(LIT 1)
(PUNCT "]")
(NEWLINE)
(DEDENT)
(KEYWORD elif)
(KEYWORD not)
(ID "final")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD del)
(ID "labels")
(PUNCT "[")
(PUNCT "-")
(LIT 1)
(PUNCT "]")
(NEWLINE)
(KEYWORD if)
(ID "labels")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "trailing_dot")
(PUNCT "=")
(LIT ".")
(NEWLINE)
(DEDENT)
(DEDENT)
(DEDENT)
(ID "result")
(PUNCT "=")
(PUNCT "[")
(PUNCT "]")
(NEWLINE)
(ID "size")
(PUNCT "=")
(LIT 0)
(NEWLINE)
(KEYWORD for)
(ID "label")
(KEYWORD in)
(ID "labels")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "result")
(PUNCT ".")
(ID "append")
(PUNCT "(")
(ID "ToUnicode")
(PUNCT "(")
(ID "label")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(ID "size")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "size")
(PUNCT "+=")
(LIT 1)
(NEWLINE)
(DEDENT)
(ID "size")
(PUNCT "+=")
(ID "len")
(PUNCT "(")
(ID "label")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "result")
(PUNCT "=")
(LIT ".")
(PUNCT ".")
(ID "join")
(PUNCT "(")
(ID "result")
(PUNCT ")")
(PUNCT "+")
(ID "trailing_dot")
(NEWLINE)
(ID "size")
(PUNCT "+=")
(ID "len")
(PUNCT "(")
(ID "trailing_dot")
(PUNCT ")")
(NEWLINE)
(KEYWORD return)
(PUNCT "(")
(ID "result")
(PUNCT ",")
(ID "size")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD class)
(ID "StreamWriter")
(PUNCT "(")
(ID "Codec")
(PUNCT ",")
(ID "codecs")
(PUNCT ".")
(ID "StreamWriter")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD pass)
(NEWLINE)
(DEDENT)
(KEYWORD class)
(ID "StreamReader")
(PUNCT "(")
(ID "Codec")
(PUNCT ",")
(ID "codecs")
(PUNCT ".")
(ID "StreamReader")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD pass)
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "getregentry")
(PUNCT "(")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(ID "codecs")
(PUNCT ".")
(ID "CodecInfo")
(PUNCT "(")
(ID "name")
(PUNCT "=")
(LIT "idna")
(PUNCT ",")
(ID "encode")
(PUNCT "=")
(ID "Codec")
(PUNCT "(")
(PUNCT ")")
(PUNCT ".")
(ID "encode")
(PUNCT ",")
(ID "decode")
(PUNCT "=")
(ID "Codec")
(PUNCT "(")
(PUNCT ")")
(PUNCT ".")
(ID "decode")
(PUNCT ",")
(ID "incrementalencoder")
(PUNCT "=")
(ID "IncrementalEncoder")
(PUNCT ",")
(ID "incrementaldecoder")
(PUNCT "=")
(ID "IncrementalDecoder")
(PUNCT ",")
(ID "streamwriter")
(PUNCT "=")
(ID "StreamWriter")
(PUNCT ",")
(ID "streamreader")
(PUNCT "=")
(ID "StreamReader")
(PUNCT ",")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ENDMARKER)
