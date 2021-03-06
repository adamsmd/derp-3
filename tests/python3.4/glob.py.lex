(LIT "Filename globbing utility.")
(NEWLINE)
(KEYWORD import)
(ID "os")
(NEWLINE)
(KEYWORD import)
(ID "re")
(NEWLINE)
(KEYWORD import)
(ID "fnmatch")
(NEWLINE)
(ID "__all__")
(PUNCT "=")
(PUNCT "[")
(LIT "glob")
(PUNCT ",")
(LIT "iglob")
(PUNCT "]")
(NEWLINE)
(KEYWORD def)
(ID "glob")
(PUNCT "(")
(ID "pathname")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Return a list of paths matching a pathname pattern.\n\n    The pattern may contain simple shell-style wildcards a la\n    fnmatch. However, unlike fnmatch, filenames starting with a\n    dot are special cases that are not matched by '*' and '?'\n    patterns.\n\n    ")
(NEWLINE)
(KEYWORD return)
(ID "list")
(PUNCT "(")
(ID "iglob")
(PUNCT "(")
(ID "pathname")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "iglob")
(PUNCT "(")
(ID "pathname")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Return an iterator which yields the paths matching a pathname pattern.\n\n    The pattern may contain simple shell-style wildcards a la\n    fnmatch. However, unlike fnmatch, filenames starting with a\n    dot are special cases that are not matched by '*' and '?'\n    patterns.\n\n    ")
(NEWLINE)
(ID "dirname")
(PUNCT ",")
(ID "basename")
(PUNCT "=")
(ID "os")
(PUNCT ".")
(ID "path")
(PUNCT ".")
(ID "split")
(PUNCT "(")
(ID "pathname")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(KEYWORD not)
(ID "has_magic")
(PUNCT "(")
(ID "pathname")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "basename")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "os")
(PUNCT ".")
(ID "path")
(PUNCT ".")
(ID "lexists")
(PUNCT "(")
(ID "pathname")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD yield)
(ID "pathname")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "os")
(PUNCT ".")
(ID "path")
(PUNCT ".")
(ID "isdir")
(PUNCT "(")
(ID "dirname")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD yield)
(ID "pathname")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD return)
(NEWLINE)
(DEDENT)
(KEYWORD if)
(KEYWORD not)
(ID "dirname")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD yield)
(KEYWORD from)
(ID "glob1")
(PUNCT "(")
(KEYWORD None)
(PUNCT ",")
(ID "basename")
(PUNCT ")")
(NEWLINE)
(KEYWORD return)
(NEWLINE)
(DEDENT)
(KEYWORD if)
(ID "dirname")
(PUNCT "!=")
(ID "pathname")
(KEYWORD and)
(ID "has_magic")
(PUNCT "(")
(ID "dirname")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "dirs")
(PUNCT "=")
(ID "iglob")
(PUNCT "(")
(ID "dirname")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "dirs")
(PUNCT "=")
(PUNCT "[")
(ID "dirname")
(PUNCT "]")
(NEWLINE)
(DEDENT)
(KEYWORD if)
(ID "has_magic")
(PUNCT "(")
(ID "basename")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "glob_in_dir")
(PUNCT "=")
(ID "glob1")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "glob_in_dir")
(PUNCT "=")
(ID "glob0")
(NEWLINE)
(DEDENT)
(KEYWORD for)
(ID "dirname")
(KEYWORD in)
(ID "dirs")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD for)
(ID "name")
(KEYWORD in)
(ID "glob_in_dir")
(PUNCT "(")
(ID "dirname")
(PUNCT ",")
(ID "basename")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD yield)
(ID "os")
(PUNCT ".")
(ID "path")
(PUNCT ".")
(ID "join")
(PUNCT "(")
(ID "dirname")
(PUNCT ",")
(ID "name")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(DEDENT)
(KEYWORD def)
(ID "glob1")
(PUNCT "(")
(ID "dirname")
(PUNCT ",")
(ID "pattern")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(KEYWORD not)
(ID "dirname")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "isinstance")
(PUNCT "(")
(ID "pattern")
(PUNCT ",")
(ID "bytes")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "dirname")
(PUNCT "=")
(ID "bytes")
(PUNCT "(")
(ID "os")
(PUNCT ".")
(ID "curdir")
(PUNCT ",")
(LIT "ASCII")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "dirname")
(PUNCT "=")
(ID "os")
(PUNCT ".")
(ID "curdir")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD try)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "names")
(PUNCT "=")
(ID "os")
(PUNCT ".")
(ID "listdir")
(PUNCT "(")
(ID "dirname")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD except)
(ID "OSError")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(PUNCT "[")
(PUNCT "]")
(NEWLINE)
(DEDENT)
(KEYWORD if)
(KEYWORD not)
(ID "_ishidden")
(PUNCT "(")
(ID "pattern")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "names")
(PUNCT "=")
(PUNCT "[")
(ID "x")
(KEYWORD for)
(ID "x")
(KEYWORD in)
(ID "names")
(KEYWORD if)
(KEYWORD not)
(ID "_ishidden")
(PUNCT "(")
(ID "x")
(PUNCT ")")
(PUNCT "]")
(NEWLINE)
(DEDENT)
(KEYWORD return)
(ID "fnmatch")
(PUNCT ".")
(ID "filter")
(PUNCT "(")
(ID "names")
(PUNCT ",")
(ID "pattern")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "glob0")
(PUNCT "(")
(ID "dirname")
(PUNCT ",")
(ID "basename")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(KEYWORD not)
(ID "basename")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "os")
(PUNCT ".")
(ID "path")
(PUNCT ".")
(ID "isdir")
(PUNCT "(")
(ID "dirname")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(PUNCT "[")
(ID "basename")
(PUNCT "]")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "os")
(PUNCT ".")
(ID "path")
(PUNCT ".")
(ID "lexists")
(PUNCT "(")
(ID "os")
(PUNCT ".")
(ID "path")
(PUNCT ".")
(ID "join")
(PUNCT "(")
(ID "dirname")
(PUNCT ",")
(ID "basename")
(PUNCT ")")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(PUNCT "[")
(ID "basename")
(PUNCT "]")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD return)
(PUNCT "[")
(PUNCT "]")
(NEWLINE)
(DEDENT)
(ID "magic_check")
(PUNCT "=")
(ID "re")
(PUNCT ".")
(ID "compile")
(PUNCT "(")
(LIT "([*?[])")
(PUNCT ")")
(NEWLINE)
(ID "magic_check_bytes")
(PUNCT "=")
(ID "re")
(PUNCT ".")
(ID "compile")
(PUNCT "(")
(LIT #"([*?[])")
(PUNCT ")")
(NEWLINE)
(KEYWORD def)
(ID "has_magic")
(PUNCT "(")
(ID "s")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "isinstance")
(PUNCT "(")
(ID "s")
(PUNCT ",")
(ID "bytes")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "match")
(PUNCT "=")
(ID "magic_check_bytes")
(PUNCT ".")
(ID "search")
(PUNCT "(")
(ID "s")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "match")
(PUNCT "=")
(ID "magic_check")
(PUNCT ".")
(ID "search")
(PUNCT "(")
(ID "s")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD return)
(ID "match")
(KEYWORD is)
(KEYWORD not)
(KEYWORD None)
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "_ishidden")
(PUNCT "(")
(ID "path")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(ID "path")
(PUNCT "[")
(LIT 0)
(PUNCT "]")
(KEYWORD in)
(PUNCT "(")
(LIT ".")
(PUNCT ",")
(LIT #".")
(PUNCT "[")
(LIT 0)
(PUNCT "]")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "escape")
(PUNCT "(")
(ID "pathname")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Escape all special characters.\n    ")
(NEWLINE)
(ID "drive")
(PUNCT ",")
(ID "pathname")
(PUNCT "=")
(ID "os")
(PUNCT ".")
(ID "path")
(PUNCT ".")
(ID "splitdrive")
(PUNCT "(")
(ID "pathname")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(ID "isinstance")
(PUNCT "(")
(ID "pathname")
(PUNCT ",")
(ID "bytes")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "pathname")
(PUNCT "=")
(ID "magic_check_bytes")
(PUNCT ".")
(ID "sub")
(PUNCT "(")
(LIT #"[\\1]")
(PUNCT ",")
(ID "pathname")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "pathname")
(PUNCT "=")
(ID "magic_check")
(PUNCT ".")
(ID "sub")
(PUNCT "(")
(LIT "[\\1]")
(PUNCT ",")
(ID "pathname")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD return)
(ID "drive")
(PUNCT "+")
(ID "pathname")
(NEWLINE)
(DEDENT)
(ENDMARKER)
