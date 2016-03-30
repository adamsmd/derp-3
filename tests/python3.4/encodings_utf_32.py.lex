(LIT "\nPython 'utf-32' Codec\n")
(NEWLINE)
(KEYWORD import)
(ID "codecs")
(PUNCT ",")
(ID "sys")
(NEWLINE)
(ID "encode")
(PUNCT "=")
(ID "codecs")
(PUNCT ".")
(ID "utf_32_encode")
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
(ID "codecs")
(PUNCT ".")
(ID "utf_32_decode")
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
(ID "__init__")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "errors")
(PUNCT "=")
(LIT "strict")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "codecs")
(PUNCT ".")
(ID "IncrementalEncoder")
(PUNCT ".")
(ID "__init__")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "errors")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "encoder")
(PUNCT "=")
(KEYWORD None)
(NEWLINE)
(DEDENT)
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
(KEYWORD if)
(ID "self")
(PUNCT ".")
(ID "encoder")
(KEYWORD is)
(KEYWORD None)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "result")
(PUNCT "=")
(ID "codecs")
(PUNCT ".")
(ID "utf_32_encode")
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
(KEYWORD if)
(ID "sys")
(PUNCT ".")
(ID "byteorder")
(PUNCT "==")
(LIT "little")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "encoder")
(PUNCT "=")
(ID "codecs")
(PUNCT ".")
(ID "utf_32_le_encode")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "encoder")
(PUNCT "=")
(ID "codecs")
(PUNCT ".")
(ID "utf_32_be_encode")
(NEWLINE)
(DEDENT)
(KEYWORD return)
(ID "result")
(NEWLINE)
(DEDENT)
(KEYWORD return)
(ID "self")
(PUNCT ".")
(ID "encoder")
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
(KEYWORD def)
(ID "reset")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "codecs")
(PUNCT ".")
(ID "IncrementalEncoder")
(PUNCT ".")
(ID "reset")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "encoder")
(PUNCT "=")
(KEYWORD None)
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "getstate")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(PUNCT "(")
(LIT 2)
(KEYWORD if)
(ID "self")
(PUNCT ".")
(ID "encoder")
(KEYWORD is)
(KEYWORD None)
(KEYWORD else)
(LIT 0)
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "setstate")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "state")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "state")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "encoder")
(PUNCT "=")
(KEYWORD None)
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "sys")
(PUNCT ".")
(ID "byteorder")
(PUNCT "==")
(LIT "little")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "encoder")
(PUNCT "=")
(ID "codecs")
(PUNCT ".")
(ID "utf_32_le_encode")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "encoder")
(PUNCT "=")
(ID "codecs")
(PUNCT ".")
(ID "utf_32_be_encode")
(NEWLINE)
(DEDENT)
(DEDENT)
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
(ID "__init__")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "errors")
(PUNCT "=")
(LIT "strict")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "codecs")
(PUNCT ".")
(ID "BufferedIncrementalDecoder")
(PUNCT ".")
(ID "__init__")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "errors")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "decoder")
(PUNCT "=")
(KEYWORD None)
(NEWLINE)
(DEDENT)
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
(ID "self")
(PUNCT ".")
(ID "decoder")
(KEYWORD is)
(KEYWORD None)
(PUNCT ":")
(NEWLINE)
(INDENT)
(PUNCT "(")
(ID "output")
(PUNCT ",")
(ID "consumed")
(PUNCT ",")
(ID "byteorder")
(PUNCT ")")
(PUNCT "=")
(ID "codecs")
(PUNCT ".")
(ID "utf_32_ex_decode")
(PUNCT "(")
(ID "input")
(PUNCT ",")
(ID "errors")
(PUNCT ",")
(LIT 0)
(PUNCT ",")
(ID "final")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(ID "byteorder")
(PUNCT "==")
(PUNCT "-")
(LIT 1)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "decoder")
(PUNCT "=")
(ID "codecs")
(PUNCT ".")
(ID "utf_32_le_decode")
(NEWLINE)
(DEDENT)
(KEYWORD elif)
(ID "byteorder")
(PUNCT "==")
(LIT 1)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "decoder")
(PUNCT "=")
(ID "codecs")
(PUNCT ".")
(ID "utf_32_be_decode")
(NEWLINE)
(DEDENT)
(KEYWORD elif)
(ID "consumed")
(PUNCT ">=")
(LIT 4)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD raise)
(ID "UnicodeError")
(PUNCT "(")
(LIT "UTF-32 stream does not start with BOM")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD return)
(PUNCT "(")
(ID "output")
(PUNCT ",")
(ID "consumed")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD return)
(ID "self")
(PUNCT ".")
(ID "decoder")
(PUNCT "(")
(ID "input")
(PUNCT ",")
(ID "self")
(PUNCT ".")
(ID "errors")
(PUNCT ",")
(ID "final")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "reset")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "codecs")
(PUNCT ".")
(ID "BufferedIncrementalDecoder")
(PUNCT ".")
(ID "reset")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "decoder")
(PUNCT "=")
(KEYWORD None)
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "getstate")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "state")
(PUNCT "=")
(ID "codecs")
(PUNCT ".")
(ID "BufferedIncrementalDecoder")
(PUNCT ".")
(ID "getstate")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT "[")
(LIT 0)
(PUNCT "]")
(NEWLINE)
(KEYWORD if)
(ID "self")
(PUNCT ".")
(ID "decoder")
(KEYWORD is)
(KEYWORD None)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(PUNCT "(")
(ID "state")
(PUNCT ",")
(LIT 2)
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "addstate")
(PUNCT "=")
(ID "int")
(PUNCT "(")
(PUNCT "(")
(ID "sys")
(PUNCT ".")
(ID "byteorder")
(PUNCT "==")
(LIT "big")
(PUNCT ")")
(PUNCT "!=")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "decoder")
(KEYWORD is)
(ID "codecs")
(PUNCT ".")
(ID "utf_32_be_decode")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(KEYWORD return)
(PUNCT "(")
(ID "state")
(PUNCT ",")
(ID "addstate")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "setstate")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "state")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "codecs")
(PUNCT ".")
(ID "BufferedIncrementalDecoder")
(PUNCT ".")
(ID "setstate")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "state")
(PUNCT ")")
(NEWLINE)
(ID "state")
(PUNCT "=")
(ID "state")
(PUNCT "[")
(LIT 1)
(PUNCT "]")
(NEWLINE)
(KEYWORD if)
(ID "state")
(PUNCT "==")
(LIT 0)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "decoder")
(PUNCT "=")
(PUNCT "(")
(ID "codecs")
(PUNCT ".")
(ID "utf_32_be_decode")
(KEYWORD if)
(ID "sys")
(PUNCT ".")
(ID "byteorder")
(PUNCT "==")
(LIT "big")
(KEYWORD else)
(ID "codecs")
(PUNCT ".")
(ID "utf_32_le_decode")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD elif)
(ID "state")
(PUNCT "==")
(LIT 1)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "decoder")
(PUNCT "=")
(PUNCT "(")
(ID "codecs")
(PUNCT ".")
(ID "utf_32_le_decode")
(KEYWORD if)
(ID "sys")
(PUNCT ".")
(ID "byteorder")
(PUNCT "==")
(LIT "big")
(KEYWORD else)
(ID "codecs")
(PUNCT ".")
(ID "utf_32_be_decode")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "decoder")
(PUNCT "=")
(KEYWORD None)
(NEWLINE)
(DEDENT)
(DEDENT)
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
(KEYWORD def)
(ID "__init__")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "stream")
(PUNCT ",")
(ID "errors")
(PUNCT "=")
(LIT "strict")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "encoder")
(PUNCT "=")
(KEYWORD None)
(NEWLINE)
(ID "codecs")
(PUNCT ".")
(ID "StreamWriter")
(PUNCT ".")
(ID "__init__")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "stream")
(PUNCT ",")
(ID "errors")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "reset")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "codecs")
(PUNCT ".")
(ID "StreamWriter")
(PUNCT ".")
(ID "reset")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "encoder")
(PUNCT "=")
(KEYWORD None)
(NEWLINE)
(DEDENT)
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
(ID "self")
(PUNCT ".")
(ID "encoder")
(KEYWORD is)
(KEYWORD None)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "result")
(PUNCT "=")
(ID "codecs")
(PUNCT ".")
(ID "utf_32_encode")
(PUNCT "(")
(ID "input")
(PUNCT ",")
(ID "errors")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(ID "sys")
(PUNCT ".")
(ID "byteorder")
(PUNCT "==")
(LIT "little")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "encoder")
(PUNCT "=")
(ID "codecs")
(PUNCT ".")
(ID "utf_32_le_encode")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "encoder")
(PUNCT "=")
(ID "codecs")
(PUNCT ".")
(ID "utf_32_be_encode")
(NEWLINE)
(DEDENT)
(KEYWORD return)
(ID "result")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(ID "self")
(PUNCT ".")
(ID "encoder")
(PUNCT "(")
(ID "input")
(PUNCT ",")
(ID "errors")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
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
(KEYWORD def)
(ID "reset")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "codecs")
(PUNCT ".")
(ID "StreamReader")
(PUNCT ".")
(ID "reset")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(NEWLINE)
(KEYWORD try)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD del)
(ID "self")
(PUNCT ".")
(ID "decode")
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
(PUNCT "(")
(ID "object")
(PUNCT ",")
(ID "consumed")
(PUNCT ",")
(ID "byteorder")
(PUNCT ")")
(PUNCT "=")
(ID "codecs")
(PUNCT ".")
(ID "utf_32_ex_decode")
(PUNCT "(")
(ID "input")
(PUNCT ",")
(ID "errors")
(PUNCT ",")
(LIT 0)
(PUNCT ",")
(KEYWORD False)
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(ID "byteorder")
(PUNCT "==")
(PUNCT "-")
(LIT 1)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "decode")
(PUNCT "=")
(ID "codecs")
(PUNCT ".")
(ID "utf_32_le_decode")
(NEWLINE)
(DEDENT)
(KEYWORD elif)
(ID "byteorder")
(PUNCT "==")
(LIT 1)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "decode")
(PUNCT "=")
(ID "codecs")
(PUNCT ".")
(ID "utf_32_be_decode")
(NEWLINE)
(DEDENT)
(KEYWORD elif)
(ID "consumed")
(PUNCT ">=")
(LIT 4)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD raise)
(ID "UnicodeError")
(PUNCT "(")
(LIT "UTF-32 stream does not start with BOM")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD return)
(PUNCT "(")
(ID "object")
(PUNCT ",")
(ID "consumed")
(PUNCT ")")
(NEWLINE)
(DEDENT)
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
(LIT "utf-32")
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
