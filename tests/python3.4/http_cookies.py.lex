(LIT "\nHere's a sample session to show how to use this module.\nAt the moment, this is the only documentation.\n\nThe Basics\n----------\n\nImporting is easy...\n\n   >>> from http import cookies\n\nMost of the time you start by creating a cookie.\n\n   >>> C = cookies.SimpleCookie()\n\nOnce you've created your Cookie, you can add values just as if it were\na dictionary.\n\n   >>> C = cookies.SimpleCookie()\n   >>> C[\"fig\"] = \"newton\"\n   >>> C[\"sugar\"] = \"wafer\"\n   >>> C.output()\n   'Set-Cookie: fig=newton\\r\\nSet-Cookie: sugar=wafer'\n\nNotice that the printable representation of a Cookie is the\nappropriate format for a Set-Cookie: header.  This is the\ndefault behavior.  You can change the header and printed\nattributes by using the .output() function\n\n   >>> C = cookies.SimpleCookie()\n   >>> C[\"rocky\"] = \"road\"\n   >>> C[\"rocky\"][\"path\"] = \"/cookie\"\n   >>> print(C.output(header=\"Cookie:\"))\n   Cookie: rocky=road; Path=/cookie\n   >>> print(C.output(attrs=[], header=\"Cookie:\"))\n   Cookie: rocky=road\n\nThe load() method of a Cookie extracts cookies from a string.  In a\nCGI script, you would use this method to extract the cookies from the\nHTTP_COOKIE environment variable.\n\n   >>> C = cookies.SimpleCookie()\n   >>> C.load(\"chips=ahoy; vienna=finger\")\n   >>> C.output()\n   'Set-Cookie: chips=ahoy\\r\\nSet-Cookie: vienna=finger'\n\nThe load() method is darn-tootin smart about identifying cookies\nwithin a string.  Escaped quotation marks, nested semicolons, and other\nsuch trickeries do not confuse it.\n\n   >>> C = cookies.SimpleCookie()\n   >>> C.load('keebler=\"E=everybody; L=\\\\\"Loves\\\\\"; fudge=\\\\012;\";')\n   >>> print(C)\n   Set-Cookie: keebler=\"E=everybody; L=\\\"Loves\\\"; fudge=\\012;\"\n\nEach element of the Cookie also supports all of the RFC 2109\nCookie attributes.  Here's an example which sets the Path\nattribute.\n\n   >>> C = cookies.SimpleCookie()\n   >>> C[\"oreo\"] = \"doublestuff\"\n   >>> C[\"oreo\"][\"path\"] = \"/\"\n   >>> print(C)\n   Set-Cookie: oreo=doublestuff; Path=/\n\nEach dictionary element has a 'value' attribute, which gives you\nback the value associated with the key.\n\n   >>> C = cookies.SimpleCookie()\n   >>> C[\"twix\"] = \"none for you\"\n   >>> C[\"twix\"].value\n   'none for you'\n\nThe SimpleCookie expects that all values should be standard strings.\nJust to be sure, SimpleCookie invokes the str() builtin to convert\nthe value to a string, when the values are set dictionary-style.\n\n   >>> C = cookies.SimpleCookie()\n   >>> C[\"number\"] = 7\n   >>> C[\"string\"] = \"seven\"\n   >>> C[\"number\"].value\n   '7'\n   >>> C[\"string\"].value\n   'seven'\n   >>> C.output()\n   'Set-Cookie: number=7\\r\\nSet-Cookie: string=seven'\n\nFinis.\n")
(NEWLINE)
(KEYWORD import)
(ID "re")
(NEWLINE)
(KEYWORD import)
(ID "string")
(NEWLINE)
(ID "__all__")
(PUNCT "=")
(PUNCT "[")
(LIT "CookieError")
(PUNCT ",")
(LIT "BaseCookie")
(PUNCT ",")
(LIT "SimpleCookie")
(PUNCT "]")
(NEWLINE)
(ID "_nulljoin")
(PUNCT "=")
(LIT "")
(PUNCT ".")
(ID "join")
(NEWLINE)
(ID "_semispacejoin")
(PUNCT "=")
(LIT "; ")
(PUNCT ".")
(ID "join")
(NEWLINE)
(ID "_spacejoin")
(PUNCT "=")
(LIT " ")
(PUNCT ".")
(ID "join")
(NEWLINE)
(KEYWORD class)
(ID "CookieError")
(PUNCT "(")
(ID "Exception")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD pass)
(NEWLINE)
(DEDENT)
(ID "_LegalChars")
(PUNCT "=")
(ID "string")
(PUNCT ".")
(ID "ascii_letters")
(PUNCT "+")
(ID "string")
(PUNCT ".")
(ID "digits")
(PUNCT "+")
(LIT "!#$%&'*+-.^_`|~:")
(NEWLINE)
(ID "_Translator")
(PUNCT "=")
(PUNCT "{")
(LIT "\u0000")
(PUNCT ":")
(LIT "\\000")
(PUNCT ",")
(LIT "\u0001")
(PUNCT ":")
(LIT "\\001")
(PUNCT ",")
(LIT "\u0002")
(PUNCT ":")
(LIT "\\002")
(PUNCT ",")
(LIT "\u0003")
(PUNCT ":")
(LIT "\\003")
(PUNCT ",")
(LIT "\u0004")
(PUNCT ":")
(LIT "\\004")
(PUNCT ",")
(LIT "\u0005")
(PUNCT ":")
(LIT "\\005")
(PUNCT ",")
(LIT "\u0006")
(PUNCT ":")
(LIT "\\006")
(PUNCT ",")
(LIT "\a")
(PUNCT ":")
(LIT "\\007")
(PUNCT ",")
(LIT "\b")
(PUNCT ":")
(LIT "\\010")
(PUNCT ",")
(LIT "\t")
(PUNCT ":")
(LIT "\\011")
(PUNCT ",")
(LIT "\n")
(PUNCT ":")
(LIT "\\012")
(PUNCT ",")
(LIT "\v")
(PUNCT ":")
(LIT "\\013")
(PUNCT ",")
(LIT "\f")
(PUNCT ":")
(LIT "\\014")
(PUNCT ",")
(LIT "\r")
(PUNCT ":")
(LIT "\\015")
(PUNCT ",")
(LIT "\u000E")
(PUNCT ":")
(LIT "\\016")
(PUNCT ",")
(LIT "\u000F")
(PUNCT ":")
(LIT "\\017")
(PUNCT ",")
(LIT "\u0010")
(PUNCT ":")
(LIT "\\020")
(PUNCT ",")
(LIT "\u0011")
(PUNCT ":")
(LIT "\\021")
(PUNCT ",")
(LIT "\u0012")
(PUNCT ":")
(LIT "\\022")
(PUNCT ",")
(LIT "\u0013")
(PUNCT ":")
(LIT "\\023")
(PUNCT ",")
(LIT "\u0014")
(PUNCT ":")
(LIT "\\024")
(PUNCT ",")
(LIT "\u0015")
(PUNCT ":")
(LIT "\\025")
(PUNCT ",")
(LIT "\u0016")
(PUNCT ":")
(LIT "\\026")
(PUNCT ",")
(LIT "\u0017")
(PUNCT ":")
(LIT "\\027")
(PUNCT ",")
(LIT "\u0018")
(PUNCT ":")
(LIT "\\030")
(PUNCT ",")
(LIT "\u0019")
(PUNCT ":")
(LIT "\\031")
(PUNCT ",")
(LIT "\u001A")
(PUNCT ":")
(LIT "\\032")
(PUNCT ",")
(LIT "\e")
(PUNCT ":")
(LIT "\\033")
(PUNCT ",")
(LIT "\u001C")
(PUNCT ":")
(LIT "\\034")
(PUNCT ",")
(LIT "\u001D")
(PUNCT ":")
(LIT "\\035")
(PUNCT ",")
(LIT "\u001E")
(PUNCT ":")
(LIT "\\036")
(PUNCT ",")
(LIT "\u001F")
(PUNCT ":")
(LIT "\\037")
(PUNCT ",")
(LIT ",")
(PUNCT ":")
(LIT "\\054")
(PUNCT ",")
(LIT ";")
(PUNCT ":")
(LIT "\\073")
(PUNCT ",")
(LIT "\"")
(PUNCT ":")
(LIT "\\\"")
(PUNCT ",")
(LIT "\\")
(PUNCT ":")
(LIT "\\\\")
(PUNCT ",")
(LIT "\u007F")
(PUNCT ":")
(LIT "\\177")
(PUNCT ",")
(LIT "\u0080")
(PUNCT ":")
(LIT "\\200")
(PUNCT ",")
(LIT "\u0081")
(PUNCT ":")
(LIT "\\201")
(PUNCT ",")
(LIT "\u0082")
(PUNCT ":")
(LIT "\\202")
(PUNCT ",")
(LIT "\u0083")
(PUNCT ":")
(LIT "\\203")
(PUNCT ",")
(LIT "\u0084")
(PUNCT ":")
(LIT "\\204")
(PUNCT ",")
(LIT "\u0085")
(PUNCT ":")
(LIT "\\205")
(PUNCT ",")
(LIT "\u0086")
(PUNCT ":")
(LIT "\\206")
(PUNCT ",")
(LIT "\u0087")
(PUNCT ":")
(LIT "\\207")
(PUNCT ",")
(LIT "\u0088")
(PUNCT ":")
(LIT "\\210")
(PUNCT ",")
(LIT "\u0089")
(PUNCT ":")
(LIT "\\211")
(PUNCT ",")
(LIT "\u008A")
(PUNCT ":")
(LIT "\\212")
(PUNCT ",")
(LIT "\u008B")
(PUNCT ":")
(LIT "\\213")
(PUNCT ",")
(LIT "\u008C")
(PUNCT ":")
(LIT "\\214")
(PUNCT ",")
(LIT "\u008D")
(PUNCT ":")
(LIT "\\215")
(PUNCT ",")
(LIT "\u008E")
(PUNCT ":")
(LIT "\\216")
(PUNCT ",")
(LIT "\u008F")
(PUNCT ":")
(LIT "\\217")
(PUNCT ",")
(LIT "\u0090")
(PUNCT ":")
(LIT "\\220")
(PUNCT ",")
(LIT "\u0091")
(PUNCT ":")
(LIT "\\221")
(PUNCT ",")
(LIT "\u0092")
(PUNCT ":")
(LIT "\\222")
(PUNCT ",")
(LIT "\u0093")
(PUNCT ":")
(LIT "\\223")
(PUNCT ",")
(LIT "\u0094")
(PUNCT ":")
(LIT "\\224")
(PUNCT ",")
(LIT "\u0095")
(PUNCT ":")
(LIT "\\225")
(PUNCT ",")
(LIT "\u0096")
(PUNCT ":")
(LIT "\\226")
(PUNCT ",")
(LIT "\u0097")
(PUNCT ":")
(LIT "\\227")
(PUNCT ",")
(LIT "\u0098")
(PUNCT ":")
(LIT "\\230")
(PUNCT ",")
(LIT "\u0099")
(PUNCT ":")
(LIT "\\231")
(PUNCT ",")
(LIT "\u009A")
(PUNCT ":")
(LIT "\\232")
(PUNCT ",")
(LIT "\u009B")
(PUNCT ":")
(LIT "\\233")
(PUNCT ",")
(LIT "\u009C")
(PUNCT ":")
(LIT "\\234")
(PUNCT ",")
(LIT "\u009D")
(PUNCT ":")
(LIT "\\235")
(PUNCT ",")
(LIT "\u009E")
(PUNCT ":")
(LIT "\\236")
(PUNCT ",")
(LIT "\u009F")
(PUNCT ":")
(LIT "\\237")
(PUNCT ",")
(LIT " ")
(PUNCT ":")
(LIT "\\240")
(PUNCT ",")
(LIT "¡")
(PUNCT ":")
(LIT "\\241")
(PUNCT ",")
(LIT "¢")
(PUNCT ":")
(LIT "\\242")
(PUNCT ",")
(LIT "£")
(PUNCT ":")
(LIT "\\243")
(PUNCT ",")
(LIT "¤")
(PUNCT ":")
(LIT "\\244")
(PUNCT ",")
(LIT "¥")
(PUNCT ":")
(LIT "\\245")
(PUNCT ",")
(LIT "¦")
(PUNCT ":")
(LIT "\\246")
(PUNCT ",")
(LIT "§")
(PUNCT ":")
(LIT "\\247")
(PUNCT ",")
(LIT "¨")
(PUNCT ":")
(LIT "\\250")
(PUNCT ",")
(LIT "©")
(PUNCT ":")
(LIT "\\251")
(PUNCT ",")
(LIT "ª")
(PUNCT ":")
(LIT "\\252")
(PUNCT ",")
(LIT "«")
(PUNCT ":")
(LIT "\\253")
(PUNCT ",")
(LIT "¬")
(PUNCT ":")
(LIT "\\254")
(PUNCT ",")
(LIT "\u00AD")
(PUNCT ":")
(LIT "\\255")
(PUNCT ",")
(LIT "®")
(PUNCT ":")
(LIT "\\256")
(PUNCT ",")
(LIT "¯")
(PUNCT ":")
(LIT "\\257")
(PUNCT ",")
(LIT "°")
(PUNCT ":")
(LIT "\\260")
(PUNCT ",")
(LIT "±")
(PUNCT ":")
(LIT "\\261")
(PUNCT ",")
(LIT "²")
(PUNCT ":")
(LIT "\\262")
(PUNCT ",")
(LIT "³")
(PUNCT ":")
(LIT "\\263")
(PUNCT ",")
(LIT "´")
(PUNCT ":")
(LIT "\\264")
(PUNCT ",")
(LIT "µ")
(PUNCT ":")
(LIT "\\265")
(PUNCT ",")
(LIT "¶")
(PUNCT ":")
(LIT "\\266")
(PUNCT ",")
(LIT "·")
(PUNCT ":")
(LIT "\\267")
(PUNCT ",")
(LIT "¸")
(PUNCT ":")
(LIT "\\270")
(PUNCT ",")
(LIT "¹")
(PUNCT ":")
(LIT "\\271")
(PUNCT ",")
(LIT "º")
(PUNCT ":")
(LIT "\\272")
(PUNCT ",")
(LIT "»")
(PUNCT ":")
(LIT "\\273")
(PUNCT ",")
(LIT "¼")
(PUNCT ":")
(LIT "\\274")
(PUNCT ",")
(LIT "½")
(PUNCT ":")
(LIT "\\275")
(PUNCT ",")
(LIT "¾")
(PUNCT ":")
(LIT "\\276")
(PUNCT ",")
(LIT "¿")
(PUNCT ":")
(LIT "\\277")
(PUNCT ",")
(LIT "À")
(PUNCT ":")
(LIT "\\300")
(PUNCT ",")
(LIT "Á")
(PUNCT ":")
(LIT "\\301")
(PUNCT ",")
(LIT "Â")
(PUNCT ":")
(LIT "\\302")
(PUNCT ",")
(LIT "Ã")
(PUNCT ":")
(LIT "\\303")
(PUNCT ",")
(LIT "Ä")
(PUNCT ":")
(LIT "\\304")
(PUNCT ",")
(LIT "Å")
(PUNCT ":")
(LIT "\\305")
(PUNCT ",")
(LIT "Æ")
(PUNCT ":")
(LIT "\\306")
(PUNCT ",")
(LIT "Ç")
(PUNCT ":")
(LIT "\\307")
(PUNCT ",")
(LIT "È")
(PUNCT ":")
(LIT "\\310")
(PUNCT ",")
(LIT "É")
(PUNCT ":")
(LIT "\\311")
(PUNCT ",")
(LIT "Ê")
(PUNCT ":")
(LIT "\\312")
(PUNCT ",")
(LIT "Ë")
(PUNCT ":")
(LIT "\\313")
(PUNCT ",")
(LIT "Ì")
(PUNCT ":")
(LIT "\\314")
(PUNCT ",")
(LIT "Í")
(PUNCT ":")
(LIT "\\315")
(PUNCT ",")
(LIT "Î")
(PUNCT ":")
(LIT "\\316")
(PUNCT ",")
(LIT "Ï")
(PUNCT ":")
(LIT "\\317")
(PUNCT ",")
(LIT "Ð")
(PUNCT ":")
(LIT "\\320")
(PUNCT ",")
(LIT "Ñ")
(PUNCT ":")
(LIT "\\321")
(PUNCT ",")
(LIT "Ò")
(PUNCT ":")
(LIT "\\322")
(PUNCT ",")
(LIT "Ó")
(PUNCT ":")
(LIT "\\323")
(PUNCT ",")
(LIT "Ô")
(PUNCT ":")
(LIT "\\324")
(PUNCT ",")
(LIT "Õ")
(PUNCT ":")
(LIT "\\325")
(PUNCT ",")
(LIT "Ö")
(PUNCT ":")
(LIT "\\326")
(PUNCT ",")
(LIT "×")
(PUNCT ":")
(LIT "\\327")
(PUNCT ",")
(LIT "Ø")
(PUNCT ":")
(LIT "\\330")
(PUNCT ",")
(LIT "Ù")
(PUNCT ":")
(LIT "\\331")
(PUNCT ",")
(LIT "Ú")
(PUNCT ":")
(LIT "\\332")
(PUNCT ",")
(LIT "Û")
(PUNCT ":")
(LIT "\\333")
(PUNCT ",")
(LIT "Ü")
(PUNCT ":")
(LIT "\\334")
(PUNCT ",")
(LIT "Ý")
(PUNCT ":")
(LIT "\\335")
(PUNCT ",")
(LIT "Þ")
(PUNCT ":")
(LIT "\\336")
(PUNCT ",")
(LIT "ß")
(PUNCT ":")
(LIT "\\337")
(PUNCT ",")
(LIT "à")
(PUNCT ":")
(LIT "\\340")
(PUNCT ",")
(LIT "á")
(PUNCT ":")
(LIT "\\341")
(PUNCT ",")
(LIT "â")
(PUNCT ":")
(LIT "\\342")
(PUNCT ",")
(LIT "ã")
(PUNCT ":")
(LIT "\\343")
(PUNCT ",")
(LIT "ä")
(PUNCT ":")
(LIT "\\344")
(PUNCT ",")
(LIT "å")
(PUNCT ":")
(LIT "\\345")
(PUNCT ",")
(LIT "æ")
(PUNCT ":")
(LIT "\\346")
(PUNCT ",")
(LIT "ç")
(PUNCT ":")
(LIT "\\347")
(PUNCT ",")
(LIT "è")
(PUNCT ":")
(LIT "\\350")
(PUNCT ",")
(LIT "é")
(PUNCT ":")
(LIT "\\351")
(PUNCT ",")
(LIT "ê")
(PUNCT ":")
(LIT "\\352")
(PUNCT ",")
(LIT "ë")
(PUNCT ":")
(LIT "\\353")
(PUNCT ",")
(LIT "ì")
(PUNCT ":")
(LIT "\\354")
(PUNCT ",")
(LIT "í")
(PUNCT ":")
(LIT "\\355")
(PUNCT ",")
(LIT "î")
(PUNCT ":")
(LIT "\\356")
(PUNCT ",")
(LIT "ï")
(PUNCT ":")
(LIT "\\357")
(PUNCT ",")
(LIT "ð")
(PUNCT ":")
(LIT "\\360")
(PUNCT ",")
(LIT "ñ")
(PUNCT ":")
(LIT "\\361")
(PUNCT ",")
(LIT "ò")
(PUNCT ":")
(LIT "\\362")
(PUNCT ",")
(LIT "ó")
(PUNCT ":")
(LIT "\\363")
(PUNCT ",")
(LIT "ô")
(PUNCT ":")
(LIT "\\364")
(PUNCT ",")
(LIT "õ")
(PUNCT ":")
(LIT "\\365")
(PUNCT ",")
(LIT "ö")
(PUNCT ":")
(LIT "\\366")
(PUNCT ",")
(LIT "÷")
(PUNCT ":")
(LIT "\\367")
(PUNCT ",")
(LIT "ø")
(PUNCT ":")
(LIT "\\370")
(PUNCT ",")
(LIT "ù")
(PUNCT ":")
(LIT "\\371")
(PUNCT ",")
(LIT "ú")
(PUNCT ":")
(LIT "\\372")
(PUNCT ",")
(LIT "û")
(PUNCT ":")
(LIT "\\373")
(PUNCT ",")
(LIT "ü")
(PUNCT ":")
(LIT "\\374")
(PUNCT ",")
(LIT "ý")
(PUNCT ":")
(LIT "\\375")
(PUNCT ",")
(LIT "þ")
(PUNCT ":")
(LIT "\\376")
(PUNCT ",")
(LIT "ÿ")
(PUNCT ":")
(LIT "\\377")
(PUNCT "}")
(NEWLINE)
(KEYWORD def)
(ID "_quote")
(PUNCT "(")
(ID "str")
(PUNCT ",")
(ID "LegalChars")
(PUNCT "=")
(ID "_LegalChars")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Quote a string for use in a cookie header.\n\n    If the string does not need to be double-quoted, then just return the\n    string.  Otherwise, surround the string in doublequotes and quote\n    (with a \\) special characters.\n    ")
(NEWLINE)
(KEYWORD if)
(ID "all")
(PUNCT "(")
(ID "c")
(KEYWORD in)
(ID "LegalChars")
(KEYWORD for)
(ID "c")
(KEYWORD in)
(ID "str")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(ID "str")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(LIT "\"")
(PUNCT "+")
(ID "_nulljoin")
(PUNCT "(")
(ID "_Translator")
(PUNCT ".")
(ID "get")
(PUNCT "(")
(ID "s")
(PUNCT ",")
(ID "s")
(PUNCT ")")
(KEYWORD for)
(ID "s")
(KEYWORD in)
(ID "str")
(PUNCT ")")
(PUNCT "+")
(LIT "\"")
(NEWLINE)
(DEDENT)
(DEDENT)
(ID "_OctalPatt")
(PUNCT "=")
(ID "re")
(PUNCT ".")
(ID "compile")
(PUNCT "(")
(LIT "\\\\[0-3][0-7][0-7]")
(PUNCT ")")
(NEWLINE)
(ID "_QuotePatt")
(PUNCT "=")
(ID "re")
(PUNCT ".")
(ID "compile")
(PUNCT "(")
(LIT "[\\\\].")
(PUNCT ")")
(NEWLINE)
(KEYWORD def)
(ID "_unquote")
(PUNCT "(")
(ID "str")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "len")
(PUNCT "(")
(ID "str")
(PUNCT ")")
(PUNCT "<")
(LIT 2)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(ID "str")
(NEWLINE)
(DEDENT)
(KEYWORD if)
(ID "str")
(PUNCT "[")
(LIT 0)
(PUNCT "]")
(PUNCT "!=")
(LIT "\"")
(KEYWORD or)
(ID "str")
(PUNCT "[")
(PUNCT "-")
(LIT 1)
(PUNCT "]")
(PUNCT "!=")
(LIT "\"")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(ID "str")
(NEWLINE)
(DEDENT)
(ID "str")
(PUNCT "=")
(ID "str")
(PUNCT "[")
(LIT 1)
(PUNCT ":")
(PUNCT "-")
(LIT 1)
(PUNCT "]")
(NEWLINE)
(ID "i")
(PUNCT "=")
(LIT 0)
(NEWLINE)
(ID "n")
(PUNCT "=")
(ID "len")
(PUNCT "(")
(ID "str")
(PUNCT ")")
(NEWLINE)
(ID "res")
(PUNCT "=")
(PUNCT "[")
(PUNCT "]")
(NEWLINE)
(KEYWORD while)
(LIT 0)
(PUNCT "<=")
(ID "i")
(PUNCT "<")
(ID "n")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "o_match")
(PUNCT "=")
(ID "_OctalPatt")
(PUNCT ".")
(ID "search")
(PUNCT "(")
(ID "str")
(PUNCT ",")
(ID "i")
(PUNCT ")")
(NEWLINE)
(ID "q_match")
(PUNCT "=")
(ID "_QuotePatt")
(PUNCT ".")
(ID "search")
(PUNCT "(")
(ID "str")
(PUNCT ",")
(ID "i")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(KEYWORD not)
(ID "o_match")
(KEYWORD and)
(KEYWORD not)
(ID "q_match")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "res")
(PUNCT ".")
(ID "append")
(PUNCT "(")
(ID "str")
(PUNCT "[")
(ID "i")
(PUNCT ":")
(PUNCT "]")
(PUNCT ")")
(NEWLINE)
(KEYWORD break)
(NEWLINE)
(DEDENT)
(ID "j")
(PUNCT "=")
(ID "k")
(PUNCT "=")
(PUNCT "-")
(LIT 1)
(NEWLINE)
(KEYWORD if)
(ID "o_match")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "j")
(PUNCT "=")
(ID "o_match")
(PUNCT ".")
(ID "start")
(PUNCT "(")
(LIT 0)
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD if)
(ID "q_match")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "k")
(PUNCT "=")
(ID "q_match")
(PUNCT ".")
(ID "start")
(PUNCT "(")
(LIT 0)
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD if)
(ID "q_match")
(KEYWORD and)
(PUNCT "(")
(KEYWORD not)
(ID "o_match")
(KEYWORD or)
(ID "k")
(PUNCT "<")
(ID "j")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "res")
(PUNCT ".")
(ID "append")
(PUNCT "(")
(ID "str")
(PUNCT "[")
(ID "i")
(PUNCT ":")
(ID "k")
(PUNCT "]")
(PUNCT ")")
(NEWLINE)
(ID "res")
(PUNCT ".")
(ID "append")
(PUNCT "(")
(ID "str")
(PUNCT "[")
(ID "k")
(PUNCT "+")
(LIT 1)
(PUNCT "]")
(PUNCT ")")
(NEWLINE)
(ID "i")
(PUNCT "=")
(ID "k")
(PUNCT "+")
(LIT 2)
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "res")
(PUNCT ".")
(ID "append")
(PUNCT "(")
(ID "str")
(PUNCT "[")
(ID "i")
(PUNCT ":")
(ID "j")
(PUNCT "]")
(PUNCT ")")
(NEWLINE)
(ID "res")
(PUNCT ".")
(ID "append")
(PUNCT "(")
(ID "chr")
(PUNCT "(")
(ID "int")
(PUNCT "(")
(ID "str")
(PUNCT "[")
(ID "j")
(PUNCT "+")
(LIT 1)
(PUNCT ":")
(ID "j")
(PUNCT "+")
(LIT 4)
(PUNCT "]")
(PUNCT ",")
(LIT 8)
(PUNCT ")")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(ID "i")
(PUNCT "=")
(ID "j")
(PUNCT "+")
(LIT 4)
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD return)
(ID "_nulljoin")
(PUNCT "(")
(ID "res")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "_weekdayname")
(PUNCT "=")
(PUNCT "[")
(LIT "Mon")
(PUNCT ",")
(LIT "Tue")
(PUNCT ",")
(LIT "Wed")
(PUNCT ",")
(LIT "Thu")
(PUNCT ",")
(LIT "Fri")
(PUNCT ",")
(LIT "Sat")
(PUNCT ",")
(LIT "Sun")
(PUNCT "]")
(NEWLINE)
(ID "_monthname")
(PUNCT "=")
(PUNCT "[")
(KEYWORD None)
(PUNCT ",")
(LIT "Jan")
(PUNCT ",")
(LIT "Feb")
(PUNCT ",")
(LIT "Mar")
(PUNCT ",")
(LIT "Apr")
(PUNCT ",")
(LIT "May")
(PUNCT ",")
(LIT "Jun")
(PUNCT ",")
(LIT "Jul")
(PUNCT ",")
(LIT "Aug")
(PUNCT ",")
(LIT "Sep")
(PUNCT ",")
(LIT "Oct")
(PUNCT ",")
(LIT "Nov")
(PUNCT ",")
(LIT "Dec")
(PUNCT "]")
(NEWLINE)
(KEYWORD def)
(ID "_getdate")
(PUNCT "(")
(ID "future")
(PUNCT "=")
(LIT 0)
(PUNCT ",")
(ID "weekdayname")
(PUNCT "=")
(ID "_weekdayname")
(PUNCT ",")
(ID "monthname")
(PUNCT "=")
(ID "_monthname")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD from)
(ID "time")
(KEYWORD import)
(ID "gmtime")
(PUNCT ",")
(ID "time")
(NEWLINE)
(ID "now")
(PUNCT "=")
(ID "time")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "year")
(PUNCT ",")
(ID "month")
(PUNCT ",")
(ID "day")
(PUNCT ",")
(ID "hh")
(PUNCT ",")
(ID "mm")
(PUNCT ",")
(ID "ss")
(PUNCT ",")
(ID "wd")
(PUNCT ",")
(ID "y")
(PUNCT ",")
(ID "z")
(PUNCT "=")
(ID "gmtime")
(PUNCT "(")
(ID "now")
(PUNCT "+")
(ID "future")
(PUNCT ")")
(NEWLINE)
(KEYWORD return)
(LIT "%s, %02d %3s %4d %02d:%02d:%02d GMT")
(PUNCT "%")
(PUNCT "(")
(ID "weekdayname")
(PUNCT "[")
(ID "wd")
(PUNCT "]")
(PUNCT ",")
(ID "day")
(PUNCT ",")
(ID "monthname")
(PUNCT "[")
(ID "month")
(PUNCT "]")
(PUNCT ",")
(ID "year")
(PUNCT ",")
(ID "hh")
(PUNCT ",")
(ID "mm")
(PUNCT ",")
(ID "ss")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD class)
(ID "Morsel")
(PUNCT "(")
(ID "dict")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "A class to hold ONE (key, value) pair.\n\n    In a cookie, each such pair may have several attributes, so this class is\n    used to keep the attributes associated with the appropriate key,value pair.\n    This class also includes a coded_value attribute, which is used to hold\n    the network representation of the value.  This is most useful when Python\n    objects are pickled for network transit.\n    ")
(NEWLINE)
(ID "_reserved")
(PUNCT "=")
(PUNCT "{")
(LIT "expires")
(PUNCT ":")
(LIT "expires")
(PUNCT ",")
(LIT "path")
(PUNCT ":")
(LIT "Path")
(PUNCT ",")
(LIT "comment")
(PUNCT ":")
(LIT "Comment")
(PUNCT ",")
(LIT "domain")
(PUNCT ":")
(LIT "Domain")
(PUNCT ",")
(LIT "max-age")
(PUNCT ":")
(LIT "Max-Age")
(PUNCT ",")
(LIT "secure")
(PUNCT ":")
(LIT "Secure")
(PUNCT ",")
(LIT "httponly")
(PUNCT ":")
(LIT "HttpOnly")
(PUNCT ",")
(LIT "version")
(PUNCT ":")
(LIT "Version")
(PUNCT ",")
(PUNCT "}")
(NEWLINE)
(ID "_flags")
(PUNCT "=")
(PUNCT "{")
(LIT "secure")
(PUNCT ",")
(LIT "httponly")
(PUNCT "}")
(NEWLINE)
(KEYWORD def)
(ID "__init__")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "key")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "value")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "coded_value")
(PUNCT "=")
(KEYWORD None)
(NEWLINE)
(KEYWORD for)
(ID "key")
(KEYWORD in)
(ID "self")
(PUNCT ".")
(ID "_reserved")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "dict")
(PUNCT ".")
(ID "__setitem__")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "key")
(PUNCT ",")
(LIT "")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD def)
(ID "__setitem__")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "K")
(PUNCT ",")
(ID "V")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "K")
(PUNCT "=")
(ID "K")
(PUNCT ".")
(ID "lower")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(KEYWORD not)
(ID "K")
(KEYWORD in)
(ID "self")
(PUNCT ".")
(ID "_reserved")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD raise)
(ID "CookieError")
(PUNCT "(")
(LIT "Invalid Attribute %s")
(PUNCT "%")
(ID "K")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "dict")
(PUNCT ".")
(ID "__setitem__")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "K")
(PUNCT ",")
(ID "V")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "isReservedKey")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "K")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(ID "K")
(PUNCT ".")
(ID "lower")
(PUNCT "(")
(PUNCT ")")
(KEYWORD in)
(ID "self")
(PUNCT ".")
(ID "_reserved")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "set")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "key")
(PUNCT ",")
(ID "val")
(PUNCT ",")
(ID "coded_val")
(PUNCT ",")
(ID "LegalChars")
(PUNCT "=")
(ID "_LegalChars")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "key")
(PUNCT ".")
(ID "lower")
(PUNCT "(")
(PUNCT ")")
(KEYWORD in)
(ID "self")
(PUNCT ".")
(ID "_reserved")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD raise)
(ID "CookieError")
(PUNCT "(")
(LIT "Attempt to set a reserved key: %s")
(PUNCT "%")
(ID "key")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD if)
(ID "any")
(PUNCT "(")
(ID "c")
(KEYWORD not)
(KEYWORD in)
(ID "LegalChars")
(KEYWORD for)
(ID "c")
(KEYWORD in)
(ID "key")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD raise)
(ID "CookieError")
(PUNCT "(")
(LIT "Illegal key value: %s")
(PUNCT "%")
(ID "key")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "self")
(PUNCT ".")
(ID "key")
(PUNCT "=")
(ID "key")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "value")
(PUNCT "=")
(ID "val")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "coded_value")
(PUNCT "=")
(ID "coded_val")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "output")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "attrs")
(PUNCT "=")
(KEYWORD None)
(PUNCT ",")
(ID "header")
(PUNCT "=")
(LIT "Set-Cookie:")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(LIT "%s %s")
(PUNCT "%")
(PUNCT "(")
(ID "header")
(PUNCT ",")
(ID "self")
(PUNCT ".")
(ID "OutputString")
(PUNCT "(")
(ID "attrs")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "__str__")
(PUNCT "=")
(ID "output")
(NEWLINE)
(KEYWORD def)
(ID "__repr__")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(LIT "<%s: %s=%s>")
(PUNCT "%")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "__class__")
(PUNCT ".")
(ID "__name__")
(PUNCT ",")
(ID "self")
(PUNCT ".")
(ID "key")
(PUNCT ",")
(ID "repr")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "value")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "js_output")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "attrs")
(PUNCT "=")
(KEYWORD None)
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(LIT "\n        <script type=\"text/javascript\">\n        <!-- begin hiding\n        document.cookie = \"%s\";\n        // end hiding -->\n        </script>\n        ")
(PUNCT "%")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "OutputString")
(PUNCT "(")
(ID "attrs")
(PUNCT ")")
(PUNCT ".")
(ID "replace")
(PUNCT "(")
(LIT "\"")
(PUNCT ",")
(LIT "\\\"")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "OutputString")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "attrs")
(PUNCT "=")
(KEYWORD None)
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "result")
(PUNCT "=")
(PUNCT "[")
(PUNCT "]")
(NEWLINE)
(ID "append")
(PUNCT "=")
(ID "result")
(PUNCT ".")
(ID "append")
(NEWLINE)
(ID "append")
(PUNCT "(")
(LIT "%s=%s")
(PUNCT "%")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "key")
(PUNCT ",")
(ID "self")
(PUNCT ".")
(ID "coded_value")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(ID "attrs")
(KEYWORD is)
(KEYWORD None)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "attrs")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "_reserved")
(NEWLINE)
(DEDENT)
(ID "items")
(PUNCT "=")
(ID "sorted")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "items")
(PUNCT "(")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(KEYWORD for)
(ID "key")
(PUNCT ",")
(ID "value")
(KEYWORD in)
(ID "items")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "value")
(PUNCT "==")
(LIT "")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD continue)
(NEWLINE)
(DEDENT)
(KEYWORD if)
(ID "key")
(KEYWORD not)
(KEYWORD in)
(ID "attrs")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD continue)
(NEWLINE)
(DEDENT)
(KEYWORD if)
(ID "key")
(PUNCT "==")
(LIT "expires")
(KEYWORD and)
(ID "isinstance")
(PUNCT "(")
(ID "value")
(PUNCT ",")
(ID "int")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "append")
(PUNCT "(")
(LIT "%s=%s")
(PUNCT "%")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "_reserved")
(PUNCT "[")
(ID "key")
(PUNCT "]")
(PUNCT ",")
(ID "_getdate")
(PUNCT "(")
(ID "value")
(PUNCT ")")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD elif)
(ID "key")
(PUNCT "==")
(LIT "max-age")
(KEYWORD and)
(ID "isinstance")
(PUNCT "(")
(ID "value")
(PUNCT ",")
(ID "int")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "append")
(PUNCT "(")
(LIT "%s=%d")
(PUNCT "%")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "_reserved")
(PUNCT "[")
(ID "key")
(PUNCT "]")
(PUNCT ",")
(ID "value")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD elif)
(ID "key")
(PUNCT "==")
(LIT "secure")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "append")
(PUNCT "(")
(ID "str")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "_reserved")
(PUNCT "[")
(ID "key")
(PUNCT "]")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD elif)
(ID "key")
(PUNCT "==")
(LIT "httponly")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "append")
(PUNCT "(")
(ID "str")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "_reserved")
(PUNCT "[")
(ID "key")
(PUNCT "]")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "append")
(PUNCT "(")
(LIT "%s=%s")
(PUNCT "%")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "_reserved")
(PUNCT "[")
(ID "key")
(PUNCT "]")
(PUNCT ",")
(ID "value")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD return)
(ID "_semispacejoin")
(PUNCT "(")
(ID "result")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(ID "_LegalCharsPatt")
(PUNCT "=")
(LIT "[\\w\\d!#%&'~_`><@,:/\\$\\*\\+\\-\\.\\^\\|\\)\\(\\?\\}\\{\\=]")
(NEWLINE)
(ID "_CookiePattern")
(PUNCT "=")
(ID "re")
(PUNCT ".")
(ID "compile")
(PUNCT "(")
(LIT "\n    (?x)                           # This is a verbose pattern\n    \\s*                            # Optional whitespace at start of cookie\n    (?P<key>                       # Start of group 'key'\n    ")
(PUNCT "+")
(ID "_LegalCharsPatt")
(PUNCT "+")
(LIT "+?   # Any word of at least one letter\n    )                              # End of group 'key'\n    (                              # Optional group: there may not be a value.\n    \\s*=\\s*                          # Equal Sign\n    (?P<val>                         # Start of group 'val'\n    \"(?:[^\\\\\"]|\\\\.)*\"                  # Any doublequoted string\n    |                                  # or\n    \\w{3},\\s[\\w\\d\\s-]{9,11}\\s[\\d:]{8}\\sGMT  # Special case for \"expires\" attr\n    |                                  # or\n    ")
(PUNCT "+")
(ID "_LegalCharsPatt")
(PUNCT "+")
(LIT "*      # Any word or empty string\n    )                                # End of group 'val'\n    )?                             # End of optional value group\n    \\s*                            # Any number of spaces.\n    (\\s+|;|$)                      # Ending either at space, semicolon, or EOS.\n    ")
(PUNCT ",")
(ID "re")
(PUNCT ".")
(ID "ASCII")
(PUNCT ")")
(NEWLINE)
(KEYWORD class)
(ID "BaseCookie")
(PUNCT "(")
(ID "dict")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "A container class for a set of Morsels.")
(NEWLINE)
(KEYWORD def)
(ID "value_decode")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "val")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "real_value, coded_value = value_decode(STRING)\n        Called prior to setting a cookie's value from the network\n        representation.  The VALUE is the value read from HTTP\n        header.\n        Override this function to modify the behavior of cookies.\n        ")
(NEWLINE)
(KEYWORD return)
(ID "val")
(PUNCT ",")
(ID "val")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "value_encode")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "val")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "real_value, coded_value = value_encode(VALUE)\n        Called prior to setting a cookie's value from the dictionary\n        representation.  The VALUE is the value being assigned.\n        Override this function to modify the behavior of cookies.\n        ")
(NEWLINE)
(ID "strval")
(PUNCT "=")
(ID "str")
(PUNCT "(")
(ID "val")
(PUNCT ")")
(NEWLINE)
(KEYWORD return)
(ID "strval")
(PUNCT ",")
(ID "strval")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "__init__")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "input")
(PUNCT "=")
(KEYWORD None)
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "input")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "load")
(PUNCT "(")
(ID "input")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD def)
(ID "__set")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "key")
(PUNCT ",")
(ID "real_value")
(PUNCT ",")
(ID "coded_value")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Private method for setting a cookie's value")
(NEWLINE)
(ID "M")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "get")
(PUNCT "(")
(ID "key")
(PUNCT ",")
(ID "Morsel")
(PUNCT "(")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(ID "M")
(PUNCT ".")
(ID "set")
(PUNCT "(")
(ID "key")
(PUNCT ",")
(ID "real_value")
(PUNCT ",")
(ID "coded_value")
(PUNCT ")")
(NEWLINE)
(ID "dict")
(PUNCT ".")
(ID "__setitem__")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "key")
(PUNCT ",")
(ID "M")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "__setitem__")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "key")
(PUNCT ",")
(ID "value")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Dictionary style assignment.")
(NEWLINE)
(KEYWORD if)
(ID "isinstance")
(PUNCT "(")
(ID "value")
(PUNCT ",")
(ID "Morsel")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "dict")
(PUNCT ".")
(ID "__setitem__")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "key")
(PUNCT ",")
(ID "value")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "rval")
(PUNCT ",")
(ID "cval")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "value_encode")
(PUNCT "(")
(ID "value")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "__set")
(PUNCT "(")
(ID "key")
(PUNCT ",")
(ID "rval")
(PUNCT ",")
(ID "cval")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD def)
(ID "output")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "attrs")
(PUNCT "=")
(KEYWORD None)
(PUNCT ",")
(ID "header")
(PUNCT "=")
(LIT "Set-Cookie:")
(PUNCT ",")
(ID "sep")
(PUNCT "=")
(LIT "\r\n")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Return a string suitable for HTTP.")
(NEWLINE)
(ID "result")
(PUNCT "=")
(PUNCT "[")
(PUNCT "]")
(NEWLINE)
(ID "items")
(PUNCT "=")
(ID "sorted")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "items")
(PUNCT "(")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(KEYWORD for)
(ID "key")
(PUNCT ",")
(ID "value")
(KEYWORD in)
(ID "items")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "result")
(PUNCT ".")
(ID "append")
(PUNCT "(")
(ID "value")
(PUNCT ".")
(ID "output")
(PUNCT "(")
(ID "attrs")
(PUNCT ",")
(ID "header")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD return)
(ID "sep")
(PUNCT ".")
(ID "join")
(PUNCT "(")
(ID "result")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "__str__")
(PUNCT "=")
(ID "output")
(NEWLINE)
(KEYWORD def)
(ID "__repr__")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "l")
(PUNCT "=")
(PUNCT "[")
(PUNCT "]")
(NEWLINE)
(ID "items")
(PUNCT "=")
(ID "sorted")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "items")
(PUNCT "(")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(KEYWORD for)
(ID "key")
(PUNCT ",")
(ID "value")
(KEYWORD in)
(ID "items")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "l")
(PUNCT ".")
(ID "append")
(PUNCT "(")
(LIT "%s=%s")
(PUNCT "%")
(PUNCT "(")
(ID "key")
(PUNCT ",")
(ID "repr")
(PUNCT "(")
(ID "value")
(PUNCT ".")
(ID "value")
(PUNCT ")")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD return)
(LIT "<%s: %s>")
(PUNCT "%")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "__class__")
(PUNCT ".")
(ID "__name__")
(PUNCT ",")
(ID "_spacejoin")
(PUNCT "(")
(ID "l")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "js_output")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "attrs")
(PUNCT "=")
(KEYWORD None)
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Return a string suitable for JavaScript.")
(NEWLINE)
(ID "result")
(PUNCT "=")
(PUNCT "[")
(PUNCT "]")
(NEWLINE)
(ID "items")
(PUNCT "=")
(ID "sorted")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "items")
(PUNCT "(")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(KEYWORD for)
(ID "key")
(PUNCT ",")
(ID "value")
(KEYWORD in)
(ID "items")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "result")
(PUNCT ".")
(ID "append")
(PUNCT "(")
(ID "value")
(PUNCT ".")
(ID "js_output")
(PUNCT "(")
(ID "attrs")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD return)
(ID "_nulljoin")
(PUNCT "(")
(ID "result")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "load")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "rawdata")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Load cookies from a string (presumably HTTP_COOKIE) or\n        from a dictionary.  Loading cookies from a dictionary 'd'\n        is equivalent to calling:\n            map(Cookie.__setitem__, d.keys(), d.values())\n        ")
(NEWLINE)
(KEYWORD if)
(ID "isinstance")
(PUNCT "(")
(ID "rawdata")
(PUNCT ",")
(ID "str")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "__parse_string")
(PUNCT "(")
(ID "rawdata")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD for)
(ID "key")
(PUNCT ",")
(ID "value")
(KEYWORD in)
(ID "rawdata")
(PUNCT ".")
(ID "items")
(PUNCT "(")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT "[")
(ID "key")
(PUNCT "]")
(PUNCT "=")
(ID "value")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD return)
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "__parse_string")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "str")
(PUNCT ",")
(ID "patt")
(PUNCT "=")
(ID "_CookiePattern")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "i")
(PUNCT "=")
(LIT 0)
(NEWLINE)
(ID "n")
(PUNCT "=")
(ID "len")
(PUNCT "(")
(ID "str")
(PUNCT ")")
(NEWLINE)
(ID "M")
(PUNCT "=")
(KEYWORD None)
(NEWLINE)
(KEYWORD while)
(LIT 0)
(PUNCT "<=")
(ID "i")
(PUNCT "<")
(ID "n")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "match")
(PUNCT "=")
(ID "patt")
(PUNCT ".")
(ID "match")
(PUNCT "(")
(ID "str")
(PUNCT ",")
(ID "i")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(KEYWORD not)
(ID "match")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD break)
(NEWLINE)
(DEDENT)
(ID "key")
(PUNCT ",")
(ID "value")
(PUNCT "=")
(ID "match")
(PUNCT ".")
(ID "group")
(PUNCT "(")
(LIT "key")
(PUNCT ")")
(PUNCT ",")
(ID "match")
(PUNCT ".")
(ID "group")
(PUNCT "(")
(LIT "val")
(PUNCT ")")
(NEWLINE)
(ID "i")
(PUNCT "=")
(ID "match")
(PUNCT ".")
(ID "end")
(PUNCT "(")
(LIT 0)
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(ID "key")
(PUNCT "[")
(LIT 0)
(PUNCT "]")
(PUNCT "==")
(LIT "$")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "M")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "M")
(PUNCT "[")
(ID "key")
(PUNCT "[")
(LIT 1)
(PUNCT ":")
(PUNCT "]")
(PUNCT "]")
(PUNCT "=")
(ID "value")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD elif)
(ID "key")
(PUNCT ".")
(ID "lower")
(PUNCT "(")
(PUNCT ")")
(KEYWORD in)
(ID "Morsel")
(PUNCT ".")
(ID "_reserved")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "M")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "value")
(KEYWORD is)
(KEYWORD None)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "key")
(PUNCT ".")
(ID "lower")
(PUNCT "(")
(PUNCT ")")
(KEYWORD in)
(ID "Morsel")
(PUNCT ".")
(ID "_flags")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "M")
(PUNCT "[")
(ID "key")
(PUNCT "]")
(PUNCT "=")
(KEYWORD True)
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "M")
(PUNCT "[")
(ID "key")
(PUNCT "]")
(PUNCT "=")
(ID "_unquote")
(PUNCT "(")
(ID "value")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(DEDENT)
(KEYWORD elif)
(ID "value")
(KEYWORD is)
(KEYWORD not)
(KEYWORD None)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "rval")
(PUNCT ",")
(ID "cval")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "value_decode")
(PUNCT "(")
(ID "value")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "__set")
(PUNCT "(")
(ID "key")
(PUNCT ",")
(ID "rval")
(PUNCT ",")
(ID "cval")
(PUNCT ")")
(NEWLINE)
(ID "M")
(PUNCT "=")
(ID "self")
(PUNCT "[")
(ID "key")
(PUNCT "]")
(NEWLINE)
(DEDENT)
(DEDENT)
(DEDENT)
(DEDENT)
(KEYWORD class)
(ID "SimpleCookie")
(PUNCT "(")
(ID "BaseCookie")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "\n    SimpleCookie supports strings as cookie values.  When setting\n    the value using the dictionary assignment notation, SimpleCookie\n    calls the builtin str() to convert the value to a string.  Values\n    received from HTTP are kept as strings.\n    ")
(NEWLINE)
(KEYWORD def)
(ID "value_decode")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "val")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(ID "_unquote")
(PUNCT "(")
(ID "val")
(PUNCT ")")
(PUNCT ",")
(ID "val")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "value_encode")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "val")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "strval")
(PUNCT "=")
(ID "str")
(PUNCT "(")
(ID "val")
(PUNCT ")")
(NEWLINE)
(KEYWORD return)
(ID "strval")
(PUNCT ",")
(ID "_quote")
(PUNCT "(")
(ID "strval")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(ENDMARKER)
