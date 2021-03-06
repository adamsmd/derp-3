(KEYWORD import)
(ID "sys")
(PUNCT ",")
(ID "os")
(NEWLINE)
(KEYWORD import)
(ID "contextlib")
(NEWLINE)
(KEYWORD import)
(ID "subprocess")
(NEWLINE)
(KEYWORD if)
(ID "os")
(PUNCT ".")
(ID "name")
(PUNCT "==")
(LIT "nt")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD def)
(ID "_get_build_version")
(PUNCT "(")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Return the version of MSVC that was used to build Python.\n\n        For Python 2.3 and up, the version number is included in\n        sys.version.  For earlier versions, assume the compiler is MSVC 6.\n        ")
(NEWLINE)
(ID "prefix")
(PUNCT "=")
(LIT "MSC v.")
(NEWLINE)
(ID "i")
(PUNCT "=")
(ID "sys")
(PUNCT ".")
(ID "version")
(PUNCT ".")
(ID "find")
(PUNCT "(")
(ID "prefix")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(ID "i")
(PUNCT "==")
(PUNCT "-")
(LIT 1)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(LIT 6)
(NEWLINE)
(DEDENT)
(ID "i")
(PUNCT "=")
(ID "i")
(PUNCT "+")
(ID "len")
(PUNCT "(")
(ID "prefix")
(PUNCT ")")
(NEWLINE)
(ID "s")
(PUNCT ",")
(ID "rest")
(PUNCT "=")
(ID "sys")
(PUNCT ".")
(ID "version")
(PUNCT "[")
(ID "i")
(PUNCT ":")
(PUNCT "]")
(PUNCT ".")
(ID "split")
(PUNCT "(")
(LIT " ")
(PUNCT ",")
(LIT 1)
(PUNCT ")")
(NEWLINE)
(ID "majorVersion")
(PUNCT "=")
(ID "int")
(PUNCT "(")
(ID "s")
(PUNCT "[")
(PUNCT ":")
(PUNCT "-")
(LIT 2)
(PUNCT "]")
(PUNCT ")")
(PUNCT "-")
(LIT 6)
(NEWLINE)
(ID "minorVersion")
(PUNCT "=")
(ID "int")
(PUNCT "(")
(ID "s")
(PUNCT "[")
(LIT 2)
(PUNCT ":")
(LIT 3)
(PUNCT "]")
(PUNCT ")")
(PUNCT "/")
(LIT 10.0)
(NEWLINE)
(KEYWORD if)
(ID "majorVersion")
(PUNCT "==")
(LIT 6)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "minorVersion")
(PUNCT "=")
(LIT 0)
(NEWLINE)
(DEDENT)
(KEYWORD if)
(ID "majorVersion")
(PUNCT ">=")
(LIT 6)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(ID "majorVersion")
(PUNCT "+")
(ID "minorVersion")
(NEWLINE)
(DEDENT)
(KEYWORD return)
(KEYWORD None)
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "find_msvcrt")
(PUNCT "(")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Return the name of the VC runtime dll")
(NEWLINE)
(ID "version")
(PUNCT "=")
(ID "_get_build_version")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(ID "version")
(KEYWORD is)
(KEYWORD None)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(KEYWORD None)
(NEWLINE)
(DEDENT)
(KEYWORD if)
(ID "version")
(PUNCT "<=")
(LIT 6)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "clibname")
(PUNCT "=")
(LIT "msvcrt")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "clibname")
(PUNCT "=")
(LIT "msvcr%d")
(PUNCT "%")
(PUNCT "(")
(ID "version")
(PUNCT "*")
(LIT 10)
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD import)
(ID "importlib")
(PUNCT ".")
(ID "machinery")
(NEWLINE)
(KEYWORD if)
(LIT "_d.pyd")
(KEYWORD in)
(ID "importlib")
(PUNCT ".")
(ID "machinery")
(PUNCT ".")
(ID "EXTENSION_SUFFIXES")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "clibname")
(PUNCT "+=")
(LIT "d")
(NEWLINE)
(DEDENT)
(KEYWORD return)
(ID "clibname")
(PUNCT "+")
(LIT ".dll")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "find_library")
(PUNCT "(")
(ID "name")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "name")
(KEYWORD in)
(PUNCT "(")
(LIT "c")
(PUNCT ",")
(LIT "m")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(ID "find_msvcrt")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD for)
(ID "directory")
(KEYWORD in)
(ID "os")
(PUNCT ".")
(ID "environ")
(PUNCT "[")
(LIT "PATH")
(PUNCT "]")
(PUNCT ".")
(ID "split")
(PUNCT "(")
(ID "os")
(PUNCT ".")
(ID "pathsep")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "fname")
(PUNCT "=")
(ID "os")
(PUNCT ".")
(ID "path")
(PUNCT ".")
(ID "join")
(PUNCT "(")
(ID "directory")
(PUNCT ",")
(ID "name")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(ID "os")
(PUNCT ".")
(ID "path")
(PUNCT ".")
(ID "isfile")
(PUNCT "(")
(ID "fname")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(ID "fname")
(NEWLINE)
(DEDENT)
(KEYWORD if)
(ID "fname")
(PUNCT ".")
(ID "lower")
(PUNCT "(")
(PUNCT ")")
(PUNCT ".")
(ID "endswith")
(PUNCT "(")
(LIT ".dll")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD continue)
(NEWLINE)
(DEDENT)
(ID "fname")
(PUNCT "=")
(ID "fname")
(PUNCT "+")
(LIT ".dll")
(NEWLINE)
(KEYWORD if)
(ID "os")
(PUNCT ".")
(ID "path")
(PUNCT ".")
(ID "isfile")
(PUNCT "(")
(ID "fname")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(ID "fname")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD return)
(KEYWORD None)
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD if)
(ID "os")
(PUNCT ".")
(ID "name")
(PUNCT "==")
(LIT "ce")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD def)
(ID "find_library")
(PUNCT "(")
(ID "name")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(ID "name")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD if)
(ID "os")
(PUNCT ".")
(ID "name")
(PUNCT "==")
(LIT "posix")
(KEYWORD and)
(ID "sys")
(PUNCT ".")
(ID "platform")
(PUNCT "==")
(LIT "darwin")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD from)
(ID "ctypes")
(PUNCT ".")
(ID "macholib")
(PUNCT ".")
(ID "dyld")
(KEYWORD import)
(ID "dyld_find")
(KEYWORD as)
(ID "_dyld_find")
(NEWLINE)
(KEYWORD def)
(ID "find_library")
(PUNCT "(")
(ID "name")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "possible")
(PUNCT "=")
(PUNCT "[")
(LIT "lib%s.dylib")
(PUNCT "%")
(ID "name")
(PUNCT ",")
(LIT "%s.dylib")
(PUNCT "%")
(ID "name")
(PUNCT ",")
(LIT "%s.framework/%s")
(PUNCT "%")
(PUNCT "(")
(ID "name")
(PUNCT ",")
(ID "name")
(PUNCT ")")
(PUNCT "]")
(NEWLINE)
(KEYWORD for)
(ID "name")
(KEYWORD in)
(ID "possible")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD try)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(ID "_dyld_find")
(PUNCT "(")
(ID "name")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD except)
(ID "ValueError")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD continue)
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD return)
(KEYWORD None)
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD elif)
(ID "os")
(PUNCT ".")
(ID "name")
(PUNCT "==")
(LIT "posix")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD import)
(ID "re")
(PUNCT ",")
(ID "tempfile")
(NEWLINE)
(KEYWORD def)
(ID "_findLib_gcc")
(PUNCT "(")
(ID "name")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "expr")
(PUNCT "=")
(LIT "[^\\(\\)\\s]*lib%s\\.[^\\(\\)\\s]*")
(PUNCT "%")
(ID "re")
(PUNCT ".")
(ID "escape")
(PUNCT "(")
(ID "name")
(PUNCT ")")
(NEWLINE)
(ID "fdout")
(PUNCT ",")
(ID "ccout")
(PUNCT "=")
(ID "tempfile")
(PUNCT ".")
(ID "mkstemp")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "os")
(PUNCT ".")
(ID "close")
(PUNCT "(")
(ID "fdout")
(PUNCT ")")
(NEWLINE)
(ID "cmd")
(PUNCT "=")
(LIT "if type gcc >/dev/null 2>&1; then CC=gcc; elif type cc >/dev/null 2>&1; then CC=cc;else exit 10; fi;")
(LIT "LANG=C LC_ALL=C $CC -Wl,-t -o ")
(PUNCT "+")
(ID "ccout")
(PUNCT "+")
(LIT " 2>&1 -l")
(PUNCT "+")
(ID "name")
(NEWLINE)
(KEYWORD try)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "f")
(PUNCT "=")
(ID "os")
(PUNCT ".")
(ID "popen")
(PUNCT "(")
(ID "cmd")
(PUNCT ")")
(NEWLINE)
(KEYWORD try)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "trace")
(PUNCT "=")
(ID "f")
(PUNCT ".")
(ID "read")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD finally)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "rv")
(PUNCT "=")
(ID "f")
(PUNCT ".")
(ID "close")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD finally)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD try)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "os")
(PUNCT ".")
(ID "unlink")
(PUNCT "(")
(ID "ccout")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD except)
(ID "FileNotFoundError")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD pass)
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD if)
(ID "rv")
(PUNCT "==")
(LIT 10)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD raise)
(ID "OSError")
(PUNCT "(")
(LIT "gcc or cc command not found")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "res")
(PUNCT "=")
(ID "re")
(PUNCT ".")
(ID "search")
(PUNCT "(")
(ID "expr")
(PUNCT ",")
(ID "trace")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(KEYWORD not)
(ID "res")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(KEYWORD None)
(NEWLINE)
(DEDENT)
(KEYWORD return)
(ID "res")
(PUNCT ".")
(ID "group")
(PUNCT "(")
(LIT 0)
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD if)
(ID "sys")
(PUNCT ".")
(ID "platform")
(PUNCT "==")
(LIT "sunos5")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD def)
(ID "_get_soname")
(PUNCT "(")
(ID "f")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(KEYWORD not)
(ID "f")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(KEYWORD None)
(NEWLINE)
(DEDENT)
(ID "cmd")
(PUNCT "=")
(LIT "/usr/ccs/bin/dump -Lpv 2>/dev/null ")
(PUNCT "+")
(ID "f")
(NEWLINE)
(KEYWORD with)
(ID "contextlib")
(PUNCT ".")
(ID "closing")
(PUNCT "(")
(ID "os")
(PUNCT ".")
(ID "popen")
(PUNCT "(")
(ID "cmd")
(PUNCT ")")
(PUNCT ")")
(KEYWORD as)
(ID "f")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "data")
(PUNCT "=")
(ID "f")
(PUNCT ".")
(ID "read")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "res")
(PUNCT "=")
(ID "re")
(PUNCT ".")
(ID "search")
(PUNCT "(")
(LIT "\\[.*\\]\\sSONAME\\s+([^\\s]+)")
(PUNCT ",")
(ID "data")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(KEYWORD not)
(ID "res")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(KEYWORD None)
(NEWLINE)
(DEDENT)
(KEYWORD return)
(ID "res")
(PUNCT ".")
(ID "group")
(PUNCT "(")
(LIT 1)
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD def)
(ID "_get_soname")
(PUNCT "(")
(ID "f")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(KEYWORD not)
(ID "f")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(KEYWORD None)
(NEWLINE)
(DEDENT)
(ID "cmd")
(PUNCT "=")
(LIT "if ! type objdump >/dev/null 2>&1; then exit 10; fi;")
(LIT "objdump -p -j .dynamic 2>/dev/null ")
(PUNCT "+")
(ID "f")
(NEWLINE)
(ID "f")
(PUNCT "=")
(ID "os")
(PUNCT ".")
(ID "popen")
(PUNCT "(")
(ID "cmd")
(PUNCT ")")
(NEWLINE)
(KEYWORD try)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "dump")
(PUNCT "=")
(ID "f")
(PUNCT ".")
(ID "read")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD finally)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "rv")
(PUNCT "=")
(ID "f")
(PUNCT ".")
(ID "close")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD if)
(ID "rv")
(PUNCT "==")
(LIT 10)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD raise)
(ID "OSError")
(PUNCT "(")
(LIT "objdump command not found")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "res")
(PUNCT "=")
(ID "re")
(PUNCT ".")
(ID "search")
(PUNCT "(")
(LIT "\\sSONAME\\s+([^\\s]+)")
(PUNCT ",")
(ID "dump")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(KEYWORD not)
(ID "res")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(KEYWORD None)
(NEWLINE)
(DEDENT)
(KEYWORD return)
(ID "res")
(PUNCT ".")
(ID "group")
(PUNCT "(")
(LIT 1)
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD if)
(ID "sys")
(PUNCT ".")
(ID "platform")
(PUNCT ".")
(ID "startswith")
(PUNCT "(")
(PUNCT "(")
(LIT "freebsd")
(PUNCT ",")
(LIT "openbsd")
(PUNCT ",")
(LIT "dragonfly")
(PUNCT ")")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD def)
(ID "_num_version")
(PUNCT "(")
(ID "libname")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "parts")
(PUNCT "=")
(ID "libname")
(PUNCT ".")
(ID "split")
(PUNCT "(")
(LIT ".")
(PUNCT ")")
(NEWLINE)
(ID "nums")
(PUNCT "=")
(PUNCT "[")
(PUNCT "]")
(NEWLINE)
(KEYWORD try)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD while)
(ID "parts")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "nums")
(PUNCT ".")
(ID "insert")
(PUNCT "(")
(LIT 0)
(PUNCT ",")
(ID "int")
(PUNCT "(")
(ID "parts")
(PUNCT ".")
(ID "pop")
(PUNCT "(")
(PUNCT ")")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD except)
(ID "ValueError")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD pass)
(NEWLINE)
(DEDENT)
(KEYWORD return)
(ID "nums")
(KEYWORD or)
(PUNCT "[")
(ID "sys")
(PUNCT ".")
(ID "maxsize")
(PUNCT "]")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "find_library")
(PUNCT "(")
(ID "name")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "ename")
(PUNCT "=")
(ID "re")
(PUNCT ".")
(ID "escape")
(PUNCT "(")
(ID "name")
(PUNCT ")")
(NEWLINE)
(ID "expr")
(PUNCT "=")
(LIT ":-l%s\\.\\S+ => \\S*/(lib%s\\.\\S+)")
(PUNCT "%")
(PUNCT "(")
(ID "ename")
(PUNCT ",")
(ID "ename")
(PUNCT ")")
(NEWLINE)
(KEYWORD with)
(ID "contextlib")
(PUNCT ".")
(ID "closing")
(PUNCT "(")
(ID "os")
(PUNCT ".")
(ID "popen")
(PUNCT "(")
(LIT "/sbin/ldconfig -r 2>/dev/null")
(PUNCT ")")
(PUNCT ")")
(KEYWORD as)
(ID "f")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "data")
(PUNCT "=")
(ID "f")
(PUNCT ".")
(ID "read")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "res")
(PUNCT "=")
(ID "re")
(PUNCT ".")
(ID "findall")
(PUNCT "(")
(ID "expr")
(PUNCT ",")
(ID "data")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(KEYWORD not)
(ID "res")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(ID "_get_soname")
(PUNCT "(")
(ID "_findLib_gcc")
(PUNCT "(")
(ID "name")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "res")
(PUNCT ".")
(ID "sort")
(PUNCT "(")
(ID "key")
(PUNCT "=")
(ID "_num_version")
(PUNCT ")")
(NEWLINE)
(KEYWORD return)
(ID "res")
(PUNCT "[")
(PUNCT "-")
(LIT 1)
(PUNCT "]")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD elif)
(ID "sys")
(PUNCT ".")
(ID "platform")
(PUNCT "==")
(LIT "sunos5")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD def)
(ID "_findLib_crle")
(PUNCT "(")
(ID "name")
(PUNCT ",")
(ID "is64")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(KEYWORD not)
(ID "os")
(PUNCT ".")
(ID "path")
(PUNCT ".")
(ID "exists")
(PUNCT "(")
(LIT "/usr/bin/crle")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(KEYWORD None)
(NEWLINE)
(DEDENT)
(KEYWORD if)
(ID "is64")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "cmd")
(PUNCT "=")
(LIT "env LC_ALL=C /usr/bin/crle -64 2>/dev/null")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "cmd")
(PUNCT "=")
(LIT "env LC_ALL=C /usr/bin/crle 2>/dev/null")
(NEWLINE)
(DEDENT)
(KEYWORD with)
(ID "contextlib")
(PUNCT ".")
(ID "closing")
(PUNCT "(")
(ID "os")
(PUNCT ".")
(ID "popen")
(PUNCT "(")
(ID "cmd")
(PUNCT ")")
(PUNCT ")")
(KEYWORD as)
(ID "f")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD for)
(ID "line")
(KEYWORD in)
(ID "f")
(PUNCT ".")
(ID "readlines")
(PUNCT "(")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "line")
(PUNCT "=")
(ID "line")
(PUNCT ".")
(ID "strip")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(ID "line")
(PUNCT ".")
(ID "startswith")
(PUNCT "(")
(LIT "Default Library Path (ELF):")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "paths")
(PUNCT "=")
(ID "line")
(PUNCT ".")
(ID "split")
(PUNCT "(")
(PUNCT ")")
(PUNCT "[")
(LIT 4)
(PUNCT "]")
(NEWLINE)
(DEDENT)
(DEDENT)
(DEDENT)
(KEYWORD if)
(KEYWORD not)
(ID "paths")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(KEYWORD None)
(NEWLINE)
(DEDENT)
(KEYWORD for)
(ID "dir")
(KEYWORD in)
(ID "paths")
(PUNCT ".")
(ID "split")
(PUNCT "(")
(LIT ":")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "libfile")
(PUNCT "=")
(ID "os")
(PUNCT ".")
(ID "path")
(PUNCT ".")
(ID "join")
(PUNCT "(")
(ID "dir")
(PUNCT ",")
(LIT "lib%s.so")
(PUNCT "%")
(ID "name")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(ID "os")
(PUNCT ".")
(ID "path")
(PUNCT ".")
(ID "exists")
(PUNCT "(")
(ID "libfile")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(ID "libfile")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD return)
(KEYWORD None)
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "find_library")
(PUNCT "(")
(ID "name")
(PUNCT ",")
(ID "is64")
(PUNCT "=")
(KEYWORD False)
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(ID "_get_soname")
(PUNCT "(")
(ID "_findLib_crle")
(PUNCT "(")
(ID "name")
(PUNCT ",")
(ID "is64")
(PUNCT ")")
(KEYWORD or)
(ID "_findLib_gcc")
(PUNCT "(")
(ID "name")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD def)
(ID "_findSoname_ldconfig")
(PUNCT "(")
(ID "name")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD import)
(ID "struct")
(NEWLINE)
(ID "uname")
(PUNCT "=")
(ID "os")
(PUNCT ".")
(ID "uname")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(ID "uname")
(PUNCT ".")
(ID "machine")
(PUNCT ".")
(ID "startswith")
(PUNCT "(")
(LIT "arm")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "machine")
(PUNCT "=")
(LIT "arm")
(NEWLINE)
(DEDENT)
(KEYWORD if)
(ID "struct")
(PUNCT ".")
(ID "calcsize")
(PUNCT "(")
(LIT "l")
(PUNCT ")")
(PUNCT "==")
(LIT 4)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "machine")
(PUNCT "=")
(ID "uname")
(PUNCT ".")
(ID "machine")
(PUNCT "+")
(LIT "-32")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "machine")
(PUNCT "=")
(ID "uname")
(PUNCT ".")
(ID "machine")
(PUNCT "+")
(LIT "-64")
(NEWLINE)
(DEDENT)
(ID "mach_map")
(PUNCT "=")
(PUNCT "{")
(LIT "x86_64-64")
(PUNCT ":")
(LIT "libc6,x86-64")
(PUNCT ",")
(LIT "ppc64-64")
(PUNCT ":")
(LIT "libc6,64bit")
(PUNCT ",")
(LIT "sparc64-64")
(PUNCT ":")
(LIT "libc6,64bit")
(PUNCT ",")
(LIT "s390x-64")
(PUNCT ":")
(LIT "libc6,64bit")
(PUNCT ",")
(LIT "ia64-64")
(PUNCT ":")
(LIT "libc6,IA-64")
(PUNCT ",")
(LIT "arm-32")
(PUNCT ":")
(LIT "libc6(,hard-float)?")
(PUNCT ",")
(PUNCT "}")
(NEWLINE)
(ID "abi_type")
(PUNCT "=")
(ID "mach_map")
(PUNCT ".")
(ID "get")
(PUNCT "(")
(ID "machine")
(PUNCT ",")
(LIT "libc6")
(PUNCT ")")
(NEWLINE)
(ID "regex")
(PUNCT "=")
(ID "os")
(PUNCT ".")
(ID "fsencode")
(PUNCT "(")
(LIT "\\s+(lib%s\\.[^\\s]+)\\s+\\(%s")
(PUNCT "%")
(PUNCT "(")
(ID "re")
(PUNCT ".")
(ID "escape")
(PUNCT "(")
(ID "name")
(PUNCT ")")
(PUNCT ",")
(ID "abi_type")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(KEYWORD try)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD with)
(ID "subprocess")
(PUNCT ".")
(ID "Popen")
(PUNCT "(")
(PUNCT "[")
(LIT "/sbin/ldconfig")
(PUNCT ",")
(LIT "-p")
(PUNCT "]")
(PUNCT ",")
(ID "stdin")
(PUNCT "=")
(ID "subprocess")
(PUNCT ".")
(ID "DEVNULL")
(PUNCT ",")
(ID "stderr")
(PUNCT "=")
(ID "subprocess")
(PUNCT ".")
(ID "DEVNULL")
(PUNCT ",")
(ID "stdout")
(PUNCT "=")
(ID "subprocess")
(PUNCT ".")
(ID "PIPE")
(PUNCT ",")
(ID "env")
(PUNCT "=")
(PUNCT "{")
(LIT "LC_ALL")
(PUNCT ":")
(LIT "C")
(PUNCT ",")
(LIT "LANG")
(PUNCT ":")
(LIT "C")
(PUNCT "}")
(PUNCT ")")
(KEYWORD as)
(ID "p")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "res")
(PUNCT "=")
(ID "re")
(PUNCT ".")
(ID "search")
(PUNCT "(")
(ID "regex")
(PUNCT ",")
(ID "p")
(PUNCT ".")
(ID "stdout")
(PUNCT ".")
(ID "read")
(PUNCT "(")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(ID "res")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(ID "os")
(PUNCT ".")
(ID "fsdecode")
(PUNCT "(")
(ID "res")
(PUNCT ".")
(ID "group")
(PUNCT "(")
(LIT 1)
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(DEDENT)
(KEYWORD except)
(ID "OSError")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD pass)
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD def)
(ID "find_library")
(PUNCT "(")
(ID "name")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(ID "_findSoname_ldconfig")
(PUNCT "(")
(ID "name")
(PUNCT ")")
(KEYWORD or)
(ID "_get_soname")
(PUNCT "(")
(ID "_findLib_gcc")
(PUNCT "(")
(ID "name")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(DEDENT)
(KEYWORD def)
(ID "test")
(PUNCT "(")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD from)
(ID "ctypes")
(KEYWORD import)
(ID "cdll")
(NEWLINE)
(KEYWORD if)
(ID "os")
(PUNCT ".")
(ID "name")
(PUNCT "==")
(LIT "nt")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "print")
(PUNCT "(")
(ID "cdll")
(PUNCT ".")
(ID "msvcrt")
(PUNCT ")")
(NEWLINE)
(ID "print")
(PUNCT "(")
(ID "cdll")
(PUNCT ".")
(ID "load")
(PUNCT "(")
(LIT "msvcrt")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(ID "print")
(PUNCT "(")
(ID "find_library")
(PUNCT "(")
(LIT "msvcrt")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD if)
(ID "os")
(PUNCT ".")
(ID "name")
(PUNCT "==")
(LIT "posix")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "print")
(PUNCT "(")
(ID "find_library")
(PUNCT "(")
(LIT "m")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(ID "print")
(PUNCT "(")
(ID "find_library")
(PUNCT "(")
(LIT "c")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(ID "print")
(PUNCT "(")
(ID "find_library")
(PUNCT "(")
(LIT "bz2")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(ID "sys")
(PUNCT ".")
(ID "platform")
(PUNCT "==")
(LIT "darwin")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "print")
(PUNCT "(")
(ID "cdll")
(PUNCT ".")
(ID "LoadLibrary")
(PUNCT "(")
(LIT "libm.dylib")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(ID "print")
(PUNCT "(")
(ID "cdll")
(PUNCT ".")
(ID "LoadLibrary")
(PUNCT "(")
(LIT "libcrypto.dylib")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(ID "print")
(PUNCT "(")
(ID "cdll")
(PUNCT ".")
(ID "LoadLibrary")
(PUNCT "(")
(LIT "libSystem.dylib")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(ID "print")
(PUNCT "(")
(ID "cdll")
(PUNCT ".")
(ID "LoadLibrary")
(PUNCT "(")
(LIT "System.framework/System")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "print")
(PUNCT "(")
(ID "cdll")
(PUNCT ".")
(ID "LoadLibrary")
(PUNCT "(")
(LIT "libm.so")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(ID "print")
(PUNCT "(")
(ID "cdll")
(PUNCT ".")
(ID "LoadLibrary")
(PUNCT "(")
(LIT "libcrypt.so")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(ID "print")
(PUNCT "(")
(ID "find_library")
(PUNCT "(")
(LIT "crypt")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(DEDENT)
(KEYWORD if)
(ID "__name__")
(PUNCT "==")
(LIT "__main__")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "test")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ENDMARKER)
