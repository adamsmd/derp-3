(LIT " Python Character Mapping Codec cp273 generated from 'python-mappings/CP273.TXT' with gencodec.py.\n\n")
(NEWLINE)
(KEYWORD import)
(ID "codecs")
(NEWLINE)
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
(KEYWORD return)
(ID "codecs")
(PUNCT ".")
(ID "charmap_encode")
(PUNCT "(")
(ID "input")
(PUNCT ",")
(ID "errors")
(PUNCT ",")
(ID "encoding_table")
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
(KEYWORD return)
(ID "codecs")
(PUNCT ".")
(ID "charmap_decode")
(PUNCT "(")
(ID "input")
(PUNCT ",")
(ID "errors")
(PUNCT ",")
(ID "decoding_table")
(PUNCT ")")
(NEWLINE)
(DEDENT)
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
(ID "codecs")
(PUNCT ".")
(ID "charmap_encode")
(PUNCT "(")
(ID "input")
(PUNCT ",")
(ID "self")
(PUNCT ".")
(ID "errors")
(PUNCT ",")
(ID "encoding_table")
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
(ID "IncrementalDecoder")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD def)
(ID "decode")
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
(ID "codecs")
(PUNCT ".")
(ID "charmap_decode")
(PUNCT "(")
(ID "input")
(PUNCT ",")
(ID "self")
(PUNCT ".")
(ID "errors")
(PUNCT ",")
(ID "decoding_table")
(PUNCT ")")
(PUNCT "[")
(LIT 0)
(PUNCT "]")
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
(LIT "cp273")
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
(ID "decoding_table")
(PUNCT "=")
(PUNCT "(")
(LIT "\u0000")
(LIT "\u0001")
(LIT "\u0002")
(LIT "\u0003")
(LIT "\u009C")
(LIT "\t")
(LIT "\u0086")
(LIT "\u007F")
(LIT "\u0097")
(LIT "\u008D")
(LIT "\u008E")
(LIT "\v")
(LIT "\f")
(LIT "\r")
(LIT "\u000E")
(LIT "\u000F")
(LIT "\u0010")
(LIT "\u0011")
(LIT "\u0012")
(LIT "\u0013")
(LIT "\u009D")
(LIT "\u0085")
(LIT "\b")
(LIT "\u0087")
(LIT "\u0018")
(LIT "\u0019")
(LIT "\u0092")
(LIT "\u008F")
(LIT "\u001C")
(LIT "\u001D")
(LIT "\u001E")
(LIT "\u001F")
(LIT "\u0080")
(LIT "\u0081")
(LIT "\u0082")
(LIT "\u0083")
(LIT "\u0084")
(LIT "\n")
(LIT "\u0017")
(LIT "\e")
(LIT "\u0088")
(LIT "\u0089")
(LIT "\u008A")
(LIT "\u008B")
(LIT "\u008C")
(LIT "\u0005")
(LIT "\u0006")
(LIT "\a")
(LIT "\u0090")
(LIT "\u0091")
(LIT "\u0016")
(LIT "\u0093")
(LIT "\u0094")
(LIT "\u0095")
(LIT "\u0096")
(LIT "\u0004")
(LIT "\u0098")
(LIT "\u0099")
(LIT "\u009A")
(LIT "\u009B")
(LIT "\u0014")
(LIT "\u0015")
(LIT "\u009E")
(LIT "\u001A")
(LIT " ")
(LIT " ")
(LIT "â")
(LIT "{")
(LIT "à")
(LIT "á")
(LIT "ã")
(LIT "å")
(LIT "ç")
(LIT "ñ")
(LIT "Ä")
(LIT ".")
(LIT "<")
(LIT "(")
(LIT "+")
(LIT "!")
(LIT "&")
(LIT "é")
(LIT "ê")
(LIT "ë")
(LIT "è")
(LIT "í")
(LIT "î")
(LIT "ï")
(LIT "ì")
(LIT "~")
(LIT "Ü")
(LIT "$")
(LIT "*")
(LIT ")")
(LIT ";")
(LIT "^")
(LIT "-")
(LIT "/")
(LIT "Â")
(LIT "[")
(LIT "À")
(LIT "Á")
(LIT "Ã")
(LIT "Å")
(LIT "Ç")
(LIT "Ñ")
(LIT "ö")
(LIT ",")
(LIT "%")
(LIT "_")
(LIT ">")
(LIT "?")
(LIT "ø")
(LIT "É")
(LIT "Ê")
(LIT "Ë")
(LIT "È")
(LIT "Í")
(LIT "Î")
(LIT "Ï")
(LIT "Ì")
(LIT "`")
(LIT ":")
(LIT "#")
(LIT "§")
(LIT "'")
(LIT "=")
(LIT "\"")
(LIT "Ø")
(LIT "a")
(LIT "b")
(LIT "c")
(LIT "d")
(LIT "e")
(LIT "f")
(LIT "g")
(LIT "h")
(LIT "i")
(LIT "«")
(LIT "»")
(LIT "ð")
(LIT "ý")
(LIT "þ")
(LIT "±")
(LIT "°")
(LIT "j")
(LIT "k")
(LIT "l")
(LIT "m")
(LIT "n")
(LIT "o")
(LIT "p")
(LIT "q")
(LIT "r")
(LIT "ª")
(LIT "º")
(LIT "æ")
(LIT "¸")
(LIT "Æ")
(LIT "¤")
(LIT "µ")
(LIT "ß")
(LIT "s")
(LIT "t")
(LIT "u")
(LIT "v")
(LIT "w")
(LIT "x")
(LIT "y")
(LIT "z")
(LIT "¡")
(LIT "¿")
(LIT "Ð")
(LIT "Ý")
(LIT "Þ")
(LIT "®")
(LIT "¢")
(LIT "£")
(LIT "¥")
(LIT "·")
(LIT "©")
(LIT "@")
(LIT "¶")
(LIT "¼")
(LIT "½")
(LIT "¾")
(LIT "¬")
(LIT "|")
(LIT "‾")
(LIT "¨")
(LIT "´")
(LIT "×")
(LIT "ä")
(LIT "A")
(LIT "B")
(LIT "C")
(LIT "D")
(LIT "E")
(LIT "F")
(LIT "G")
(LIT "H")
(LIT "I")
(LIT "\u00AD")
(LIT "ô")
(LIT "¦")
(LIT "ò")
(LIT "ó")
(LIT "õ")
(LIT "ü")
(LIT "J")
(LIT "K")
(LIT "L")
(LIT "M")
(LIT "N")
(LIT "O")
(LIT "P")
(LIT "Q")
(LIT "R")
(LIT "¹")
(LIT "û")
(LIT "}")
(LIT "ù")
(LIT "ú")
(LIT "ÿ")
(LIT "Ö")
(LIT "÷")
(LIT "S")
(LIT "T")
(LIT "U")
(LIT "V")
(LIT "W")
(LIT "X")
(LIT "Y")
(LIT "Z")
(LIT "²")
(LIT "Ô")
(LIT "\\")
(LIT "Ò")
(LIT "Ó")
(LIT "Õ")
(LIT "0")
(LIT "1")
(LIT "2")
(LIT "3")
(LIT "4")
(LIT "5")
(LIT "6")
(LIT "7")
(LIT "8")
(LIT "9")
(LIT "³")
(LIT "Û")
(LIT "]")
(LIT "Ù")
(LIT "Ú")
(LIT "\u009F")
(PUNCT ")")
(NEWLINE)
(ID "encoding_table")
(PUNCT "=")
(ID "codecs")
(PUNCT ".")
(ID "charmap_build")
(PUNCT "(")
(ID "decoding_table")
(PUNCT ")")
(NEWLINE)
(ENDMARKER)
