(LIT " Standard \"encodings\" Package\n\n    Standard Python encoding modules are stored in this package\n    directory.\n\n    Codec modules must have names corresponding to normalized encoding\n    names as defined in the normalize_encoding() function below, e.g.\n    'utf-8' must be implemented by the module 'utf_8.py'.\n\n    Each codec module must export the following interface:\n\n    * getregentry() -> codecs.CodecInfo object\n    The getregentry() API must return a CodecInfo object with encoder, decoder,\n    incrementalencoder, incrementaldecoder, streamwriter and streamreader\n    atttributes which adhere to the Python Codec Interface Standard.\n\n    In addition, a module may optionally also define the following\n    APIs which are then used by the package's codec search function:\n\n    * getaliases() -> sequence of encoding name strings to use as aliases\n\n    Alias names returned by getaliases() must be normalized encoding\n    names as defined by normalize_encoding().\n\nWritten by Marc-Andre Lemburg (mal@lemburg.com).\n\n(c) Copyright CNRI, All Rights Reserved. NO WARRANTY.\n\n")
(NEWLINE)
(KEYWORD import)
(ID "codecs")
(NEWLINE)
(KEYWORD from)
(PUNCT ".")
(KEYWORD import)
(ID "aliases")
(NEWLINE)
(ID "_cache")
(PUNCT "=")
(PUNCT "{")
(PUNCT "}")
(NEWLINE)
(ID "_unknown")
(PUNCT "=")
(LIT "--unknown--")
(NEWLINE)
(ID "_import_tail")
(PUNCT "=")
(PUNCT "[")
(LIT "*")
(PUNCT "]")
(NEWLINE)
(ID "_aliases")
(PUNCT "=")
(ID "aliases")
(PUNCT ".")
(ID "aliases")
(NEWLINE)
(KEYWORD class)
(ID "CodecRegistryError")
(PUNCT "(")
(ID "LookupError")
(PUNCT ",")
(ID "SystemError")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD pass)
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "normalize_encoding")
(PUNCT "(")
(ID "encoding")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT " Normalize an encoding name.\n\n        Normalization works as follows: all non-alphanumeric\n        characters except the dot used for Python package names are\n        collapsed and replaced with a single underscore, e.g. '  -;#'\n        becomes '_'. Leading and trailing underscores are removed.\n\n        Note that encoding names should be ASCII only; if they do use\n        non-ASCII characters, these must be Latin-1 compatible.\n\n    ")
(NEWLINE)
(KEYWORD if)
(ID "isinstance")
(PUNCT "(")
(ID "encoding")
(PUNCT ",")
(ID "bytes")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "encoding")
(PUNCT "=")
(ID "str")
(PUNCT "(")
(ID "encoding")
(PUNCT ",")
(LIT "ascii")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "chars")
(PUNCT "=")
(PUNCT "[")
(PUNCT "]")
(NEWLINE)
(ID "punct")
(PUNCT "=")
(KEYWORD False)
(NEWLINE)
(KEYWORD for)
(ID "c")
(KEYWORD in)
(ID "encoding")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "c")
(PUNCT ".")
(ID "isalnum")
(PUNCT "(")
(PUNCT ")")
(KEYWORD or)
(ID "c")
(PUNCT "==")
(LIT ".")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "punct")
(KEYWORD and)
(ID "chars")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "chars")
(PUNCT ".")
(ID "append")
(PUNCT "(")
(LIT "_")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "chars")
(PUNCT ".")
(ID "append")
(PUNCT "(")
(ID "c")
(PUNCT ")")
(NEWLINE)
(ID "punct")
(PUNCT "=")
(KEYWORD False)
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "punct")
(PUNCT "=")
(KEYWORD True)
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD return)
(LIT "")
(PUNCT ".")
(ID "join")
(PUNCT "(")
(ID "chars")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "search_function")
(PUNCT "(")
(ID "encoding")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "entry")
(PUNCT "=")
(ID "_cache")
(PUNCT ".")
(ID "get")
(PUNCT "(")
(ID "encoding")
(PUNCT ",")
(ID "_unknown")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(ID "entry")
(KEYWORD is)
(KEYWORD not)
(ID "_unknown")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(ID "entry")
(NEWLINE)
(DEDENT)
(ID "norm_encoding")
(PUNCT "=")
(ID "normalize_encoding")
(PUNCT "(")
(ID "encoding")
(PUNCT ")")
(NEWLINE)
(ID "aliased_encoding")
(PUNCT "=")
(ID "_aliases")
(PUNCT ".")
(ID "get")
(PUNCT "(")
(ID "norm_encoding")
(PUNCT ")")
(KEYWORD or)
(ID "_aliases")
(PUNCT ".")
(ID "get")
(PUNCT "(")
(ID "norm_encoding")
(PUNCT ".")
(ID "replace")
(PUNCT "(")
(LIT ".")
(PUNCT ",")
(LIT "_")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(ID "aliased_encoding")
(KEYWORD is)
(KEYWORD not)
(KEYWORD None)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "modnames")
(PUNCT "=")
(PUNCT "[")
(ID "aliased_encoding")
(PUNCT ",")
(ID "norm_encoding")
(PUNCT "]")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "modnames")
(PUNCT "=")
(PUNCT "[")
(ID "norm_encoding")
(PUNCT "]")
(NEWLINE)
(DEDENT)
(KEYWORD for)
(ID "modname")
(KEYWORD in)
(ID "modnames")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(KEYWORD not)
(ID "modname")
(KEYWORD or)
(LIT ".")
(KEYWORD in)
(ID "modname")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD continue)
(NEWLINE)
(DEDENT)
(KEYWORD try)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "mod")
(PUNCT "=")
(ID "__import__")
(PUNCT "(")
(LIT "encodings.")
(PUNCT "+")
(ID "modname")
(PUNCT ",")
(ID "fromlist")
(PUNCT "=")
(ID "_import_tail")
(PUNCT ",")
(ID "level")
(PUNCT "=")
(LIT 0)
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD except)
(ID "ImportError")
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
(KEYWORD break)
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "mod")
(PUNCT "=")
(KEYWORD None)
(NEWLINE)
(DEDENT)
(KEYWORD try)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "getregentry")
(PUNCT "=")
(ID "mod")
(PUNCT ".")
(ID "getregentry")
(NEWLINE)
(DEDENT)
(KEYWORD except)
(ID "AttributeError")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "mod")
(PUNCT "=")
(KEYWORD None)
(NEWLINE)
(DEDENT)
(KEYWORD if)
(ID "mod")
(KEYWORD is)
(KEYWORD None)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "_cache")
(PUNCT "[")
(ID "encoding")
(PUNCT "]")
(PUNCT "=")
(KEYWORD None)
(NEWLINE)
(KEYWORD return)
(KEYWORD None)
(NEWLINE)
(DEDENT)
(ID "entry")
(PUNCT "=")
(ID "getregentry")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(KEYWORD not)
(ID "isinstance")
(PUNCT "(")
(ID "entry")
(PUNCT ",")
(ID "codecs")
(PUNCT ".")
(ID "CodecInfo")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(KEYWORD not)
(LIT 4)
(PUNCT "<=")
(ID "len")
(PUNCT "(")
(ID "entry")
(PUNCT ")")
(PUNCT "<=")
(LIT 7)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD raise)
(ID "CodecRegistryError")
(PUNCT "(")
(LIT "module \"%s\" (%s) failed to register")
(PUNCT "%")
(PUNCT "(")
(ID "mod")
(PUNCT ".")
(ID "__name__")
(PUNCT ",")
(ID "mod")
(PUNCT ".")
(ID "__file__")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD if)
(KEYWORD not)
(ID "callable")
(PUNCT "(")
(ID "entry")
(PUNCT "[")
(LIT 0)
(PUNCT "]")
(PUNCT ")")
(KEYWORD or)
(KEYWORD not)
(ID "callable")
(PUNCT "(")
(ID "entry")
(PUNCT "[")
(LIT 1)
(PUNCT "]")
(PUNCT ")")
(KEYWORD or)
(PUNCT "(")
(ID "entry")
(PUNCT "[")
(LIT 2)
(PUNCT "]")
(KEYWORD is)
(KEYWORD not)
(KEYWORD None)
(KEYWORD and)
(KEYWORD not)
(ID "callable")
(PUNCT "(")
(ID "entry")
(PUNCT "[")
(LIT 2)
(PUNCT "]")
(PUNCT ")")
(PUNCT ")")
(KEYWORD or)
(PUNCT "(")
(ID "entry")
(PUNCT "[")
(LIT 3)
(PUNCT "]")
(KEYWORD is)
(KEYWORD not)
(KEYWORD None)
(KEYWORD and)
(KEYWORD not)
(ID "callable")
(PUNCT "(")
(ID "entry")
(PUNCT "[")
(LIT 3)
(PUNCT "]")
(PUNCT ")")
(PUNCT ")")
(KEYWORD or)
(PUNCT "(")
(ID "len")
(PUNCT "(")
(ID "entry")
(PUNCT ")")
(PUNCT ">")
(LIT 4)
(KEYWORD and)
(ID "entry")
(PUNCT "[")
(LIT 4)
(PUNCT "]")
(KEYWORD is)
(KEYWORD not)
(KEYWORD None)
(KEYWORD and)
(KEYWORD not)
(ID "callable")
(PUNCT "(")
(ID "entry")
(PUNCT "[")
(LIT 4)
(PUNCT "]")
(PUNCT ")")
(PUNCT ")")
(KEYWORD or)
(PUNCT "(")
(ID "len")
(PUNCT "(")
(ID "entry")
(PUNCT ")")
(PUNCT ">")
(LIT 5)
(KEYWORD and)
(ID "entry")
(PUNCT "[")
(LIT 5)
(PUNCT "]")
(KEYWORD is)
(KEYWORD not)
(KEYWORD None)
(KEYWORD and)
(KEYWORD not)
(ID "callable")
(PUNCT "(")
(ID "entry")
(PUNCT "[")
(LIT 5)
(PUNCT "]")
(PUNCT ")")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD raise)
(ID "CodecRegistryError")
(PUNCT "(")
(LIT "incompatible codecs in module \"%s\" (%s)")
(PUNCT "%")
(PUNCT "(")
(ID "mod")
(PUNCT ".")
(ID "__name__")
(PUNCT ",")
(ID "mod")
(PUNCT ".")
(ID "__file__")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD if)
(ID "len")
(PUNCT "(")
(ID "entry")
(PUNCT ")")
(PUNCT "<")
(LIT 7)
(KEYWORD or)
(ID "entry")
(PUNCT "[")
(LIT 6)
(PUNCT "]")
(KEYWORD is)
(KEYWORD None)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "entry")
(PUNCT "+=")
(PUNCT "(")
(KEYWORD None)
(PUNCT ",")
(PUNCT ")")
(PUNCT "*")
(PUNCT "(")
(LIT 6)
(PUNCT "-")
(ID "len")
(PUNCT "(")
(ID "entry")
(PUNCT ")")
(PUNCT ")")
(PUNCT "+")
(PUNCT "(")
(ID "mod")
(PUNCT ".")
(ID "__name__")
(PUNCT ".")
(ID "split")
(PUNCT "(")
(LIT ".")
(PUNCT ",")
(LIT 1)
(PUNCT ")")
(PUNCT "[")
(LIT 1)
(PUNCT "]")
(PUNCT ",")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "entry")
(PUNCT "=")
(ID "codecs")
(PUNCT ".")
(ID "CodecInfo")
(PUNCT "(")
(PUNCT "*")
(ID "entry")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "_cache")
(PUNCT "[")
(ID "encoding")
(PUNCT "]")
(PUNCT "=")
(ID "entry")
(NEWLINE)
(KEYWORD try)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "codecaliases")
(PUNCT "=")
(ID "mod")
(PUNCT ".")
(ID "getaliases")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD except)
(ID "AttributeError")
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
(KEYWORD for)
(ID "alias")
(KEYWORD in)
(ID "codecaliases")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "alias")
(KEYWORD not)
(KEYWORD in)
(ID "_aliases")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "_aliases")
(PUNCT "[")
(ID "alias")
(PUNCT "]")
(PUNCT "=")
(ID "modname")
(NEWLINE)
(DEDENT)
(DEDENT)
(DEDENT)
(KEYWORD return)
(ID "entry")
(NEWLINE)
(DEDENT)
(ID "codecs")
(PUNCT ".")
(ID "register")
(PUNCT "(")
(ID "search_function")
(PUNCT ")")
(NEWLINE)
(ENDMARKER)