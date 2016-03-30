(LIT " Python 'mbcs' Codec for Windows\n\n\nCloned by Mark Hammond (mhammond@skippinet.com.au) from ascii.py,\nwhich was written by Marc-Andre Lemburg (mal@lemburg.com).\n\n(c) Copyright CNRI, All Rights Reserved. NO WARRANTY.\n\n")
(NEWLINE)
(KEYWORD from)
(ID "codecs")
(KEYWORD import)
(ID "mbcs_encode")
(PUNCT ",")
(ID "mbcs_decode")
(NEWLINE)
(KEYWORD import)
(ID "codecs")
(NEWLINE)
(ID "encode")
(PUNCT "=")
(ID "mbcs_encode")
(NEWLINE)
(KEYWORD def)
(ID "decode")
(PUNCT "(")
(ID "input")
(PUNCT ",")
(ID "errors")
(PUNCT "=")
(LIT "strict")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(ID "mbcs_decode")
(PUNCT "(")
(ID "input")
(PUNCT ",")
(ID "errors")
(PUNCT ",")
(KEYWORD True)
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD class)
(ID "IncrementalEncoder")
(PUNCT "(")
(ID "codecs")
(PUNCT ".")
(ID "IncrementalEncoder")
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
(ID "final")
(PUNCT "=")
(KEYWORD False)
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(ID "mbcs_encode")
(PUNCT "(")
(ID "input")
(PUNCT ",")
(ID "self")
(PUNCT ".")
(ID "errors")
(PUNCT ")")
(PUNCT "[")
(LIT 0)
(PUNCT "]")
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
(ID "_buffer_decode")
(PUNCT "=")
(ID "mbcs_decode")
(NEWLINE)
(DEDENT)
(KEYWORD class)
(ID "StreamWriter")
(PUNCT "(")
(ID "codecs")
(PUNCT ".")
(ID "StreamWriter")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "encode")
(PUNCT "=")
(ID "mbcs_encode")
(NEWLINE)
(DEDENT)
(KEYWORD class)
(ID "StreamReader")
(PUNCT "(")
(ID "codecs")
(PUNCT ".")
(ID "StreamReader")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "decode")
(PUNCT "=")
(ID "mbcs_decode")
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
(LIT "mbcs")
(PUNCT ",")
(ID "encode")
(PUNCT "=")
(ID "encode")
(PUNCT ",")
(ID "decode")
(PUNCT "=")
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
(ID "streamreader")
(PUNCT "=")
(ID "StreamReader")
(PUNCT ",")
(ID "streamwriter")
(PUNCT "=")
(ID "StreamWriter")
(PUNCT ",")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ENDMARKER)
