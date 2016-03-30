(LIT " Python Character Mapping Codec iso8859_5 generated from 'MAPPINGS/ISO8859/8859-5.TXT' with gencodec.py.\n\n")
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
(LIT "iso8859-5")
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
(LIT "\u0004")
(LIT "\u0005")
(LIT "\u0006")
(LIT "\a")
(LIT "\b")
(LIT "\t")
(LIT "\n")
(LIT "\v")
(LIT "\f")
(LIT "\r")
(LIT "\u000E")
(LIT "\u000F")
(LIT "\u0010")
(LIT "\u0011")
(LIT "\u0012")
(LIT "\u0013")
(LIT "\u0014")
(LIT "\u0015")
(LIT "\u0016")
(LIT "\u0017")
(LIT "\u0018")
(LIT "\u0019")
(LIT "\u001A")
(LIT "\e")
(LIT "\u001C")
(LIT "\u001D")
(LIT "\u001E")
(LIT "\u001F")
(LIT " ")
(LIT "!")
(LIT "\"")
(LIT "#")
(LIT "$")
(LIT "%")
(LIT "&")
(LIT "'")
(LIT "(")
(LIT ")")
(LIT "*")
(LIT "+")
(LIT ",")
(LIT "-")
(LIT ".")
(LIT "/")
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
(LIT ":")
(LIT ";")
(LIT "<")
(LIT "=")
(LIT ">")
(LIT "?")
(LIT "@")
(LIT "A")
(LIT "B")
(LIT "C")
(LIT "D")
(LIT "E")
(LIT "F")
(LIT "G")
(LIT "H")
(LIT "I")
(LIT "J")
(LIT "K")
(LIT "L")
(LIT "M")
(LIT "N")
(LIT "O")
(LIT "P")
(LIT "Q")
(LIT "R")
(LIT "S")
(LIT "T")
(LIT "U")
(LIT "V")
(LIT "W")
(LIT "X")
(LIT "Y")
(LIT "Z")
(LIT "[")
(LIT "\\")
(LIT "]")
(LIT "^")
(LIT "_")
(LIT "`")
(LIT "a")
(LIT "b")
(LIT "c")
(LIT "d")
(LIT "e")
(LIT "f")
(LIT "g")
(LIT "h")
(LIT "i")
(LIT "j")
(LIT "k")
(LIT "l")
(LIT "m")
(LIT "n")
(LIT "o")
(LIT "p")
(LIT "q")
(LIT "r")
(LIT "s")
(LIT "t")
(LIT "u")
(LIT "v")
(LIT "w")
(LIT "x")
(LIT "y")
(LIT "z")
(LIT "{")
(LIT "|")
(LIT "}")
(LIT "~")
(LIT "\u007F")
(LIT "\u0080")
(LIT "\u0081")
(LIT "\u0082")
(LIT "\u0083")
(LIT "\u0084")
(LIT "\u0085")
(LIT "\u0086")
(LIT "\u0087")
(LIT "\u0088")
(LIT "\u0089")
(LIT "\u008A")
(LIT "\u008B")
(LIT "\u008C")
(LIT "\u008D")
(LIT "\u008E")
(LIT "\u008F")
(LIT "\u0090")
(LIT "\u0091")
(LIT "\u0092")
(LIT "\u0093")
(LIT "\u0094")
(LIT "\u0095")
(LIT "\u0096")
(LIT "\u0097")
(LIT "\u0098")
(LIT "\u0099")
(LIT "\u009A")
(LIT "\u009B")
(LIT "\u009C")
(LIT "\u009D")
(LIT "\u009E")
(LIT "\u009F")
(LIT " ")
(LIT "Ё")
(LIT "Ђ")
(LIT "Ѓ")
(LIT "Є")
(LIT "Ѕ")
(LIT "І")
(LIT "Ї")
(LIT "Ј")
(LIT "Љ")
(LIT "Њ")
(LIT "Ћ")
(LIT "Ќ")
(LIT "\u00AD")
(LIT "Ў")
(LIT "Џ")
(LIT "А")
(LIT "Б")
(LIT "В")
(LIT "Г")
(LIT "Д")
(LIT "Е")
(LIT "Ж")
(LIT "З")
(LIT "И")
(LIT "Й")
(LIT "К")
(LIT "Л")
(LIT "М")
(LIT "Н")
(LIT "О")
(LIT "П")
(LIT "Р")
(LIT "С")
(LIT "Т")
(LIT "У")
(LIT "Ф")
(LIT "Х")
(LIT "Ц")
(LIT "Ч")
(LIT "Ш")
(LIT "Щ")
(LIT "Ъ")
(LIT "Ы")
(LIT "Ь")
(LIT "Э")
(LIT "Ю")
(LIT "Я")
(LIT "а")
(LIT "б")
(LIT "в")
(LIT "г")
(LIT "д")
(LIT "е")
(LIT "ж")
(LIT "з")
(LIT "и")
(LIT "й")
(LIT "к")
(LIT "л")
(LIT "м")
(LIT "н")
(LIT "о")
(LIT "п")
(LIT "р")
(LIT "с")
(LIT "т")
(LIT "у")
(LIT "ф")
(LIT "х")
(LIT "ц")
(LIT "ч")
(LIT "ш")
(LIT "щ")
(LIT "ъ")
(LIT "ы")
(LIT "ь")
(LIT "э")
(LIT "ю")
(LIT "я")
(LIT "№")
(LIT "ё")
(LIT "ђ")
(LIT "ѓ")
(LIT "є")
(LIT "ѕ")
(LIT "і")
(LIT "ї")
(LIT "ј")
(LIT "љ")
(LIT "њ")
(LIT "ћ")
(LIT "ќ")
(LIT "§")
(LIT "ў")
(LIT "џ")
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