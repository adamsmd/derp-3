(KEYWORD import)
(ID "copy")
(NEWLINE)
(KEYWORD from)
(PUNCT ".")
(KEYWORD import)
(ID "ElementTree")
(NEWLINE)
(ID "XINCLUDE")
(PUNCT "=")
(LIT "{http://www.w3.org/2001/XInclude}")
(NEWLINE)
(ID "XINCLUDE_INCLUDE")
(PUNCT "=")
(ID "XINCLUDE")
(PUNCT "+")
(LIT "include")
(NEWLINE)
(ID "XINCLUDE_FALLBACK")
(PUNCT "=")
(ID "XINCLUDE")
(PUNCT "+")
(LIT "fallback")
(NEWLINE)
(KEYWORD class)
(ID "FatalIncludeError")
(PUNCT "(")
(ID "SyntaxError")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD pass)
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "default_loader")
(PUNCT "(")
(ID "href")
(PUNCT ",")
(ID "parse")
(PUNCT ",")
(ID "encoding")
(PUNCT "=")
(KEYWORD None)
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "parse")
(PUNCT "==")
(LIT "xml")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD with)
(ID "open")
(PUNCT "(")
(ID "href")
(PUNCT ",")
(LIT "rb")
(PUNCT ")")
(KEYWORD as)
(ID "file")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "data")
(PUNCT "=")
(ID "ElementTree")
(PUNCT ".")
(ID "parse")
(PUNCT "(")
(ID "file")
(PUNCT ")")
(PUNCT ".")
(ID "getroot")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(KEYWORD not)
(ID "encoding")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "encoding")
(PUNCT "=")
(LIT "UTF-8")
(NEWLINE)
(DEDENT)
(KEYWORD with)
(ID "open")
(PUNCT "(")
(ID "href")
(PUNCT ",")
(LIT "r")
(PUNCT ",")
(ID "encoding")
(PUNCT "=")
(ID "encoding")
(PUNCT ")")
(KEYWORD as)
(ID "file")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "data")
(PUNCT "=")
(ID "file")
(PUNCT ".")
(ID "read")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD return)
(ID "data")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "include")
(PUNCT "(")
(ID "elem")
(PUNCT ",")
(ID "loader")
(PUNCT "=")
(KEYWORD None)
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "loader")
(KEYWORD is)
(KEYWORD None)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "loader")
(PUNCT "=")
(ID "default_loader")
(NEWLINE)
(DEDENT)
(ID "i")
(PUNCT "=")
(LIT 0)
(NEWLINE)
(KEYWORD while)
(ID "i")
(PUNCT "<")
(ID "len")
(PUNCT "(")
(ID "elem")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "e")
(PUNCT "=")
(ID "elem")
(PUNCT "[")
(ID "i")
(PUNCT "]")
(NEWLINE)
(KEYWORD if)
(ID "e")
(PUNCT ".")
(ID "tag")
(PUNCT "==")
(ID "XINCLUDE_INCLUDE")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "href")
(PUNCT "=")
(ID "e")
(PUNCT ".")
(ID "get")
(PUNCT "(")
(LIT "href")
(PUNCT ")")
(NEWLINE)
(ID "parse")
(PUNCT "=")
(ID "e")
(PUNCT ".")
(ID "get")
(PUNCT "(")
(LIT "parse")
(PUNCT ",")
(LIT "xml")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(ID "parse")
(PUNCT "==")
(LIT "xml")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "node")
(PUNCT "=")
(ID "loader")
(PUNCT "(")
(ID "href")
(PUNCT ",")
(ID "parse")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(ID "node")
(KEYWORD is)
(KEYWORD None)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD raise)
(ID "FatalIncludeError")
(PUNCT "(")
(LIT "cannot load %r as %r")
(PUNCT "%")
(PUNCT "(")
(ID "href")
(PUNCT ",")
(ID "parse")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "node")
(PUNCT "=")
(ID "copy")
(PUNCT ".")
(ID "copy")
(PUNCT "(")
(ID "node")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(ID "e")
(PUNCT ".")
(ID "tail")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "node")
(PUNCT ".")
(ID "tail")
(PUNCT "=")
(PUNCT "(")
(ID "node")
(PUNCT ".")
(ID "tail")
(KEYWORD or)
(LIT "")
(PUNCT ")")
(PUNCT "+")
(ID "e")
(PUNCT ".")
(ID "tail")
(NEWLINE)
(DEDENT)
(ID "elem")
(PUNCT "[")
(ID "i")
(PUNCT "]")
(PUNCT "=")
(ID "node")
(NEWLINE)
(DEDENT)
(KEYWORD elif)
(ID "parse")
(PUNCT "==")
(LIT "text")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "text")
(PUNCT "=")
(ID "loader")
(PUNCT "(")
(ID "href")
(PUNCT ",")
(ID "parse")
(PUNCT ",")
(ID "e")
(PUNCT ".")
(ID "get")
(PUNCT "(")
(LIT "encoding")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(ID "text")
(KEYWORD is)
(KEYWORD None)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD raise)
(ID "FatalIncludeError")
(PUNCT "(")
(LIT "cannot load %r as %r")
(PUNCT "%")
(PUNCT "(")
(ID "href")
(PUNCT ",")
(ID "parse")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD if)
(ID "i")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "node")
(PUNCT "=")
(ID "elem")
(PUNCT "[")
(ID "i")
(PUNCT "-")
(LIT 1)
(PUNCT "]")
(NEWLINE)
(ID "node")
(PUNCT ".")
(ID "tail")
(PUNCT "=")
(PUNCT "(")
(ID "node")
(PUNCT ".")
(ID "tail")
(KEYWORD or)
(LIT "")
(PUNCT ")")
(PUNCT "+")
(ID "text")
(PUNCT "+")
(PUNCT "(")
(ID "e")
(PUNCT ".")
(ID "tail")
(KEYWORD or)
(LIT "")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "elem")
(PUNCT ".")
(ID "text")
(PUNCT "=")
(PUNCT "(")
(ID "elem")
(PUNCT ".")
(ID "text")
(KEYWORD or)
(LIT "")
(PUNCT ")")
(PUNCT "+")
(ID "text")
(PUNCT "+")
(PUNCT "(")
(ID "e")
(PUNCT ".")
(ID "tail")
(KEYWORD or)
(LIT "")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD del)
(ID "elem")
(PUNCT "[")
(ID "i")
(PUNCT "]")
(NEWLINE)
(KEYWORD continue)
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD raise)
(ID "FatalIncludeError")
(PUNCT "(")
(LIT "unknown parse type in xi:include tag (%r)")
(PUNCT "%")
(ID "parse")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD elif)
(ID "e")
(PUNCT ".")
(ID "tag")
(PUNCT "==")
(ID "XINCLUDE_FALLBACK")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD raise)
(ID "FatalIncludeError")
(PUNCT "(")
(LIT "xi:fallback tag must be child of xi:include (%r)")
(PUNCT "%")
(ID "e")
(PUNCT ".")
(ID "tag")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "include")
(PUNCT "(")
(ID "e")
(PUNCT ",")
(ID "loader")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "i")
(PUNCT "=")
(ID "i")
(PUNCT "+")
(LIT 1)
(NEWLINE)
(DEDENT)
(DEDENT)
(ENDMARKER)
