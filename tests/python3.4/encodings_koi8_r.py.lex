(LIT " Python Character Mapping Codec koi8_r generated from 'MAPPINGS/VENDORS/MISC/KOI8-R.TXT' with gencodec.py.\n\n")
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
(LIT "koi8-r")
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
(LIT "─")
(LIT "│")
(LIT "┌")
(LIT "┐")
(LIT "└")
(LIT "┘")
(LIT "├")
(LIT "┤")
(LIT "┬")
(LIT "┴")
(LIT "┼")
(LIT "▀")
(LIT "▄")
(LIT "█")
(LIT "▌")
(LIT "▐")
(LIT "░")
(LIT "▒")
(LIT "▓")
(LIT "⌠")
(LIT "■")
(LIT "∙")
(LIT "√")
(LIT "≈")
(LIT "≤")
(LIT "≥")
(LIT " ")
(LIT "⌡")
(LIT "°")
(LIT "²")
(LIT "·")
(LIT "÷")
(LIT "═")
(LIT "║")
(LIT "╒")
(LIT "ё")
(LIT "╓")
(LIT "╔")
(LIT "╕")
(LIT "╖")
(LIT "╗")
(LIT "╘")
(LIT "╙")
(LIT "╚")
(LIT "╛")
(LIT "╜")
(LIT "╝")
(LIT "╞")
(LIT "╟")
(LIT "╠")
(LIT "╡")
(LIT "Ё")
(LIT "╢")
(LIT "╣")
(LIT "╤")
(LIT "╥")
(LIT "╦")
(LIT "╧")
(LIT "╨")
(LIT "╩")
(LIT "╪")
(LIT "╫")
(LIT "╬")
(LIT "©")
(LIT "ю")
(LIT "а")
(LIT "б")
(LIT "ц")
(LIT "д")
(LIT "е")
(LIT "ф")
(LIT "г")
(LIT "х")
(LIT "и")
(LIT "й")
(LIT "к")
(LIT "л")
(LIT "м")
(LIT "н")
(LIT "о")
(LIT "п")
(LIT "я")
(LIT "р")
(LIT "с")
(LIT "т")
(LIT "у")
(LIT "ж")
(LIT "в")
(LIT "ь")
(LIT "ы")
(LIT "з")
(LIT "ш")
(LIT "э")
(LIT "щ")
(LIT "ч")
(LIT "ъ")
(LIT "Ю")
(LIT "А")
(LIT "Б")
(LIT "Ц")
(LIT "Д")
(LIT "Е")
(LIT "Ф")
(LIT "Г")
(LIT "Х")
(LIT "И")
(LIT "Й")
(LIT "К")
(LIT "Л")
(LIT "М")
(LIT "Н")
(LIT "О")
(LIT "П")
(LIT "Я")
(LIT "Р")
(LIT "С")
(LIT "Т")
(LIT "У")
(LIT "Ж")
(LIT "В")
(LIT "Ь")
(LIT "Ы")
(LIT "З")
(LIT "Ш")
(LIT "Э")
(LIT "Щ")
(LIT "Ч")
(LIT "Ъ")
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
