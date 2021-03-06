(LIT "Routines to help recognizing sound files.\n\nFunction whathdr() recognizes various types of sound file headers.\nIt understands almost all headers that SOX can decode.\n\nThe return tuple contains the following items, in this order:\n- file type (as SOX understands it)\n- sampling rate (0 if unknown or hard to decode)\n- number of channels (0 if unknown or hard to decode)\n- number of frames in the file (-1 if unknown or hard to decode)\n- number of bits/sample, or 'U' for U-LAW, or 'A' for A-LAW\n\nIf the file doesn't have a recognizable type, it returns None.\nIf the file can't be opened, OSError is raised.\n\nTo compute the total time, divide the number of frames by the\nsampling rate (a frame contains a sample for each channel).\n\nFunction what() calls whathdr().  (It used to also use some\nheuristics for raw data, but this doesn't work very well.)\n\nFinally, the function test() is a simple main program that calls\nwhat() for all files mentioned on the argument list.  For directory\narguments it calls what() for all files in that directory.  Default\nargument is \".\" (testing all files in the current directory).  The\noption -r tells it to recurse down directories found inside\nexplicitly given directories.\n")
(NEWLINE)
(ID "__all__")
(PUNCT "=")
(PUNCT "[")
(LIT "what")
(PUNCT ",")
(LIT "whathdr")
(PUNCT "]")
(NEWLINE)
(KEYWORD def)
(ID "what")
(PUNCT "(")
(ID "filename")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Guess the type of a sound file.")
(NEWLINE)
(ID "res")
(PUNCT "=")
(ID "whathdr")
(PUNCT "(")
(ID "filename")
(PUNCT ")")
(NEWLINE)
(KEYWORD return)
(ID "res")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "whathdr")
(PUNCT "(")
(ID "filename")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Recognize sound headers.")
(NEWLINE)
(KEYWORD with)
(ID "open")
(PUNCT "(")
(ID "filename")
(PUNCT ",")
(LIT "rb")
(PUNCT ")")
(KEYWORD as)
(ID "f")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "h")
(PUNCT "=")
(ID "f")
(PUNCT ".")
(ID "read")
(PUNCT "(")
(LIT 512)
(PUNCT ")")
(NEWLINE)
(KEYWORD for)
(ID "tf")
(KEYWORD in)
(ID "tests")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "res")
(PUNCT "=")
(ID "tf")
(PUNCT "(")
(ID "h")
(PUNCT ",")
(ID "f")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(ID "res")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(ID "res")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD return)
(KEYWORD None)
(NEWLINE)
(DEDENT)
(DEDENT)
(ID "tests")
(PUNCT "=")
(PUNCT "[")
(PUNCT "]")
(NEWLINE)
(KEYWORD def)
(ID "test_aifc")
(PUNCT "(")
(ID "h")
(PUNCT ",")
(ID "f")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD import)
(ID "aifc")
(NEWLINE)
(KEYWORD if)
(KEYWORD not)
(ID "h")
(PUNCT ".")
(ID "startswith")
(PUNCT "(")
(LIT #"FORM")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(KEYWORD None)
(NEWLINE)
(DEDENT)
(KEYWORD if)
(ID "h")
(PUNCT "[")
(LIT 8)
(PUNCT ":")
(LIT 12)
(PUNCT "]")
(PUNCT "==")
(LIT #"AIFC")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "fmt")
(PUNCT "=")
(LIT "aifc")
(NEWLINE)
(DEDENT)
(KEYWORD elif)
(ID "h")
(PUNCT "[")
(LIT 8)
(PUNCT ":")
(LIT 12)
(PUNCT "]")
(PUNCT "==")
(LIT #"AIFF")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "fmt")
(PUNCT "=")
(LIT "aiff")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(KEYWORD None)
(NEWLINE)
(DEDENT)
(ID "f")
(PUNCT ".")
(ID "seek")
(PUNCT "(")
(LIT 0)
(PUNCT ")")
(NEWLINE)
(KEYWORD try)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "a")
(PUNCT "=")
(ID "aifc")
(PUNCT ".")
(ID "open")
(PUNCT "(")
(ID "f")
(PUNCT ",")
(LIT "r")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD except)
(PUNCT "(")
(ID "EOFError")
(PUNCT ",")
(ID "aifc")
(PUNCT ".")
(ID "Error")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(KEYWORD None)
(NEWLINE)
(DEDENT)
(KEYWORD return)
(PUNCT "(")
(ID "fmt")
(PUNCT ",")
(ID "a")
(PUNCT ".")
(ID "getframerate")
(PUNCT "(")
(PUNCT ")")
(PUNCT ",")
(ID "a")
(PUNCT ".")
(ID "getnchannels")
(PUNCT "(")
(PUNCT ")")
(PUNCT ",")
(ID "a")
(PUNCT ".")
(ID "getnframes")
(PUNCT "(")
(PUNCT ")")
(PUNCT ",")
(LIT 8)
(PUNCT "*")
(ID "a")
(PUNCT ".")
(ID "getsampwidth")
(PUNCT "(")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "tests")
(PUNCT ".")
(ID "append")
(PUNCT "(")
(ID "test_aifc")
(PUNCT ")")
(NEWLINE)
(KEYWORD def)
(ID "test_au")
(PUNCT "(")
(ID "h")
(PUNCT ",")
(ID "f")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "h")
(PUNCT ".")
(ID "startswith")
(PUNCT "(")
(LIT #".snd")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "func")
(PUNCT "=")
(ID "get_long_be")
(NEWLINE)
(DEDENT)
(KEYWORD elif)
(ID "h")
(PUNCT "[")
(PUNCT ":")
(LIT 4)
(PUNCT "]")
(KEYWORD in)
(PUNCT "(")
(LIT #"\0ds.")
(PUNCT ",")
(LIT #"dns.")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "func")
(PUNCT "=")
(ID "get_long_le")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(KEYWORD None)
(NEWLINE)
(DEDENT)
(ID "filetype")
(PUNCT "=")
(LIT "au")
(NEWLINE)
(ID "hdr_size")
(PUNCT "=")
(ID "func")
(PUNCT "(")
(ID "h")
(PUNCT "[")
(LIT 4)
(PUNCT ":")
(LIT 8)
(PUNCT "]")
(PUNCT ")")
(NEWLINE)
(ID "data_size")
(PUNCT "=")
(ID "func")
(PUNCT "(")
(ID "h")
(PUNCT "[")
(LIT 8)
(PUNCT ":")
(LIT 12)
(PUNCT "]")
(PUNCT ")")
(NEWLINE)
(ID "encoding")
(PUNCT "=")
(ID "func")
(PUNCT "(")
(ID "h")
(PUNCT "[")
(LIT 12)
(PUNCT ":")
(LIT 16)
(PUNCT "]")
(PUNCT ")")
(NEWLINE)
(ID "rate")
(PUNCT "=")
(ID "func")
(PUNCT "(")
(ID "h")
(PUNCT "[")
(LIT 16)
(PUNCT ":")
(LIT 20)
(PUNCT "]")
(PUNCT ")")
(NEWLINE)
(ID "nchannels")
(PUNCT "=")
(ID "func")
(PUNCT "(")
(ID "h")
(PUNCT "[")
(LIT 20)
(PUNCT ":")
(LIT 24)
(PUNCT "]")
(PUNCT ")")
(NEWLINE)
(ID "sample_size")
(PUNCT "=")
(LIT 1)
(NEWLINE)
(KEYWORD if)
(ID "encoding")
(PUNCT "==")
(LIT 1)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "sample_bits")
(PUNCT "=")
(LIT "U")
(NEWLINE)
(DEDENT)
(KEYWORD elif)
(ID "encoding")
(PUNCT "==")
(LIT 2)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "sample_bits")
(PUNCT "=")
(LIT 8)
(NEWLINE)
(DEDENT)
(KEYWORD elif)
(ID "encoding")
(PUNCT "==")
(LIT 3)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "sample_bits")
(PUNCT "=")
(LIT 16)
(NEWLINE)
(ID "sample_size")
(PUNCT "=")
(LIT 2)
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "sample_bits")
(PUNCT "=")
(LIT "?")
(NEWLINE)
(DEDENT)
(ID "frame_size")
(PUNCT "=")
(ID "sample_size")
(PUNCT "*")
(ID "nchannels")
(NEWLINE)
(KEYWORD if)
(ID "frame_size")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "nframe")
(PUNCT "=")
(ID "data_size")
(PUNCT "/")
(ID "frame_size")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "nframe")
(PUNCT "=")
(PUNCT "-")
(LIT 1)
(NEWLINE)
(DEDENT)
(KEYWORD return)
(ID "filetype")
(PUNCT ",")
(ID "rate")
(PUNCT ",")
(ID "nchannels")
(PUNCT ",")
(ID "nframe")
(PUNCT ",")
(ID "sample_bits")
(NEWLINE)
(DEDENT)
(ID "tests")
(PUNCT ".")
(ID "append")
(PUNCT "(")
(ID "test_au")
(PUNCT ")")
(NEWLINE)
(KEYWORD def)
(ID "test_hcom")
(PUNCT "(")
(ID "h")
(PUNCT ",")
(ID "f")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "h")
(PUNCT "[")
(LIT 65)
(PUNCT ":")
(LIT 69)
(PUNCT "]")
(PUNCT "!=")
(LIT #"FSSD")
(KEYWORD or)
(ID "h")
(PUNCT "[")
(LIT 128)
(PUNCT ":")
(LIT 132)
(PUNCT "]")
(PUNCT "!=")
(LIT #"HCOM")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(KEYWORD None)
(NEWLINE)
(DEDENT)
(ID "divisor")
(PUNCT "=")
(ID "get_long_be")
(PUNCT "(")
(ID "h")
(PUNCT "[")
(LIT 144)
(PUNCT ":")
(LIT 148)
(PUNCT "]")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(ID "divisor")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "rate")
(PUNCT "=")
(LIT 22050)
(PUNCT "/")
(ID "divisor")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "rate")
(PUNCT "=")
(LIT 0)
(NEWLINE)
(DEDENT)
(KEYWORD return)
(LIT "hcom")
(PUNCT ",")
(ID "rate")
(PUNCT ",")
(LIT 1)
(PUNCT ",")
(PUNCT "-")
(LIT 1)
(PUNCT ",")
(LIT 8)
(NEWLINE)
(DEDENT)
(ID "tests")
(PUNCT ".")
(ID "append")
(PUNCT "(")
(ID "test_hcom")
(PUNCT ")")
(NEWLINE)
(KEYWORD def)
(ID "test_voc")
(PUNCT "(")
(ID "h")
(PUNCT ",")
(ID "f")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(KEYWORD not)
(ID "h")
(PUNCT ".")
(ID "startswith")
(PUNCT "(")
(LIT #"Creative Voice File\32")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(KEYWORD None)
(NEWLINE)
(DEDENT)
(ID "sbseek")
(PUNCT "=")
(ID "get_short_le")
(PUNCT "(")
(ID "h")
(PUNCT "[")
(LIT 20)
(PUNCT ":")
(LIT 22)
(PUNCT "]")
(PUNCT ")")
(NEWLINE)
(ID "rate")
(PUNCT "=")
(LIT 0)
(NEWLINE)
(KEYWORD if)
(LIT 0)
(PUNCT "<=")
(ID "sbseek")
(PUNCT "<")
(LIT 500)
(KEYWORD and)
(ID "h")
(PUNCT "[")
(ID "sbseek")
(PUNCT "]")
(PUNCT "==")
(LIT 1)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "ratecode")
(PUNCT "=")
(LIT 256)
(PUNCT "-")
(ID "h")
(PUNCT "[")
(ID "sbseek")
(PUNCT "+")
(LIT 4)
(PUNCT "]")
(NEWLINE)
(KEYWORD if)
(ID "ratecode")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "rate")
(PUNCT "=")
(ID "int")
(PUNCT "(")
(LIT 1000000.0)
(PUNCT "/")
(ID "ratecode")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD return)
(LIT "voc")
(PUNCT ",")
(ID "rate")
(PUNCT ",")
(LIT 1)
(PUNCT ",")
(PUNCT "-")
(LIT 1)
(PUNCT ",")
(LIT 8)
(NEWLINE)
(DEDENT)
(ID "tests")
(PUNCT ".")
(ID "append")
(PUNCT "(")
(ID "test_voc")
(PUNCT ")")
(NEWLINE)
(KEYWORD def)
(ID "test_wav")
(PUNCT "(")
(ID "h")
(PUNCT ",")
(ID "f")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD import)
(ID "wave")
(NEWLINE)
(KEYWORD if)
(KEYWORD not)
(ID "h")
(PUNCT ".")
(ID "startswith")
(PUNCT "(")
(LIT #"RIFF")
(PUNCT ")")
(KEYWORD or)
(ID "h")
(PUNCT "[")
(LIT 8)
(PUNCT ":")
(LIT 12)
(PUNCT "]")
(PUNCT "!=")
(LIT #"WAVE")
(KEYWORD or)
(ID "h")
(PUNCT "[")
(LIT 12)
(PUNCT ":")
(LIT 16)
(PUNCT "]")
(PUNCT "!=")
(LIT #"fmt ")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(KEYWORD None)
(NEWLINE)
(DEDENT)
(ID "f")
(PUNCT ".")
(ID "seek")
(PUNCT "(")
(LIT 0)
(PUNCT ")")
(NEWLINE)
(KEYWORD try)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "w")
(PUNCT "=")
(ID "wave")
(PUNCT ".")
(ID "openfp")
(PUNCT "(")
(ID "f")
(PUNCT ",")
(LIT "r")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD except)
(PUNCT "(")
(ID "EOFError")
(PUNCT ",")
(ID "wave")
(PUNCT ".")
(ID "Error")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(KEYWORD None)
(NEWLINE)
(DEDENT)
(KEYWORD return)
(PUNCT "(")
(LIT "wav")
(PUNCT ",")
(ID "w")
(PUNCT ".")
(ID "getframerate")
(PUNCT "(")
(PUNCT ")")
(PUNCT ",")
(ID "w")
(PUNCT ".")
(ID "getnchannels")
(PUNCT "(")
(PUNCT ")")
(PUNCT ",")
(ID "w")
(PUNCT ".")
(ID "getnframes")
(PUNCT "(")
(PUNCT ")")
(PUNCT ",")
(LIT 8)
(PUNCT "*")
(ID "w")
(PUNCT ".")
(ID "getsampwidth")
(PUNCT "(")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "tests")
(PUNCT ".")
(ID "append")
(PUNCT "(")
(ID "test_wav")
(PUNCT ")")
(NEWLINE)
(KEYWORD def)
(ID "test_8svx")
(PUNCT "(")
(ID "h")
(PUNCT ",")
(ID "f")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(KEYWORD not)
(ID "h")
(PUNCT ".")
(ID "startswith")
(PUNCT "(")
(LIT #"FORM")
(PUNCT ")")
(KEYWORD or)
(ID "h")
(PUNCT "[")
(LIT 8)
(PUNCT ":")
(LIT 12)
(PUNCT "]")
(PUNCT "!=")
(LIT #"8SVX")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(KEYWORD None)
(NEWLINE)
(DEDENT)
(KEYWORD return)
(LIT "8svx")
(PUNCT ",")
(LIT 0)
(PUNCT ",")
(LIT 1)
(PUNCT ",")
(LIT 0)
(PUNCT ",")
(LIT 8)
(NEWLINE)
(DEDENT)
(ID "tests")
(PUNCT ".")
(ID "append")
(PUNCT "(")
(ID "test_8svx")
(PUNCT ")")
(NEWLINE)
(KEYWORD def)
(ID "test_sndt")
(PUNCT "(")
(ID "h")
(PUNCT ",")
(ID "f")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "h")
(PUNCT ".")
(ID "startswith")
(PUNCT "(")
(LIT #"SOUND")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "nsamples")
(PUNCT "=")
(ID "get_long_le")
(PUNCT "(")
(ID "h")
(PUNCT "[")
(LIT 8)
(PUNCT ":")
(LIT 12)
(PUNCT "]")
(PUNCT ")")
(NEWLINE)
(ID "rate")
(PUNCT "=")
(ID "get_short_le")
(PUNCT "(")
(ID "h")
(PUNCT "[")
(LIT 20)
(PUNCT ":")
(LIT 22)
(PUNCT "]")
(PUNCT ")")
(NEWLINE)
(KEYWORD return)
(LIT "sndt")
(PUNCT ",")
(ID "rate")
(PUNCT ",")
(LIT 1)
(PUNCT ",")
(ID "nsamples")
(PUNCT ",")
(LIT 8)
(NEWLINE)
(DEDENT)
(DEDENT)
(ID "tests")
(PUNCT ".")
(ID "append")
(PUNCT "(")
(ID "test_sndt")
(PUNCT ")")
(NEWLINE)
(KEYWORD def)
(ID "test_sndr")
(PUNCT "(")
(ID "h")
(PUNCT ",")
(ID "f")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "h")
(PUNCT ".")
(ID "startswith")
(PUNCT "(")
(LIT #"\0\0")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "rate")
(PUNCT "=")
(ID "get_short_le")
(PUNCT "(")
(ID "h")
(PUNCT "[")
(LIT 2)
(PUNCT ":")
(LIT 4)
(PUNCT "]")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(LIT 4000)
(PUNCT "<=")
(ID "rate")
(PUNCT "<=")
(LIT 25000)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(LIT "sndr")
(PUNCT ",")
(ID "rate")
(PUNCT ",")
(LIT 1)
(PUNCT ",")
(PUNCT "-")
(LIT 1)
(PUNCT ",")
(LIT 8)
(NEWLINE)
(DEDENT)
(DEDENT)
(DEDENT)
(ID "tests")
(PUNCT ".")
(ID "append")
(PUNCT "(")
(ID "test_sndr")
(PUNCT ")")
(NEWLINE)
(KEYWORD def)
(ID "get_long_be")
(PUNCT "(")
(ID "b")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(PUNCT "(")
(ID "b")
(PUNCT "[")
(LIT 0)
(PUNCT "]")
(PUNCT "<<")
(LIT 24)
(PUNCT ")")
(PUNCT "|")
(PUNCT "(")
(ID "b")
(PUNCT "[")
(LIT 1)
(PUNCT "]")
(PUNCT "<<")
(LIT 16)
(PUNCT ")")
(PUNCT "|")
(PUNCT "(")
(ID "b")
(PUNCT "[")
(LIT 2)
(PUNCT "]")
(PUNCT "<<")
(LIT 8)
(PUNCT ")")
(PUNCT "|")
(ID "b")
(PUNCT "[")
(LIT 3)
(PUNCT "]")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "get_long_le")
(PUNCT "(")
(ID "b")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(PUNCT "(")
(ID "b")
(PUNCT "[")
(LIT 3)
(PUNCT "]")
(PUNCT "<<")
(LIT 24)
(PUNCT ")")
(PUNCT "|")
(PUNCT "(")
(ID "b")
(PUNCT "[")
(LIT 2)
(PUNCT "]")
(PUNCT "<<")
(LIT 16)
(PUNCT ")")
(PUNCT "|")
(PUNCT "(")
(ID "b")
(PUNCT "[")
(LIT 1)
(PUNCT "]")
(PUNCT "<<")
(LIT 8)
(PUNCT ")")
(PUNCT "|")
(ID "b")
(PUNCT "[")
(LIT 0)
(PUNCT "]")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "get_short_be")
(PUNCT "(")
(ID "b")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(PUNCT "(")
(ID "b")
(PUNCT "[")
(LIT 0)
(PUNCT "]")
(PUNCT "<<")
(LIT 8)
(PUNCT ")")
(PUNCT "|")
(ID "b")
(PUNCT "[")
(LIT 1)
(PUNCT "]")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "get_short_le")
(PUNCT "(")
(ID "b")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(PUNCT "(")
(ID "b")
(PUNCT "[")
(LIT 1)
(PUNCT "]")
(PUNCT "<<")
(LIT 8)
(PUNCT ")")
(PUNCT "|")
(ID "b")
(PUNCT "[")
(LIT 0)
(PUNCT "]")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "test")
(PUNCT "(")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD import)
(ID "sys")
(NEWLINE)
(ID "recursive")
(PUNCT "=")
(LIT 0)
(NEWLINE)
(KEYWORD if)
(ID "sys")
(PUNCT ".")
(ID "argv")
(PUNCT "[")
(LIT 1)
(PUNCT ":")
(PUNCT "]")
(KEYWORD and)
(ID "sys")
(PUNCT ".")
(ID "argv")
(PUNCT "[")
(LIT 1)
(PUNCT "]")
(PUNCT "==")
(LIT "-r")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD del)
(ID "sys")
(PUNCT ".")
(ID "argv")
(PUNCT "[")
(LIT 1)
(PUNCT ":")
(LIT 2)
(PUNCT "]")
(NEWLINE)
(ID "recursive")
(PUNCT "=")
(LIT 1)
(NEWLINE)
(DEDENT)
(KEYWORD try)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "sys")
(PUNCT ".")
(ID "argv")
(PUNCT "[")
(LIT 1)
(PUNCT ":")
(PUNCT "]")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "testall")
(PUNCT "(")
(ID "sys")
(PUNCT ".")
(ID "argv")
(PUNCT "[")
(LIT 1)
(PUNCT ":")
(PUNCT "]")
(PUNCT ",")
(ID "recursive")
(PUNCT ",")
(LIT 1)
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "testall")
(PUNCT "(")
(PUNCT "[")
(LIT ".")
(PUNCT "]")
(PUNCT ",")
(ID "recursive")
(PUNCT ",")
(LIT 1)
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD except)
(ID "KeyboardInterrupt")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "sys")
(PUNCT ".")
(ID "stderr")
(PUNCT ".")
(ID "write")
(PUNCT "(")
(LIT "\n[Interrupted]\n")
(PUNCT ")")
(NEWLINE)
(ID "sys")
(PUNCT ".")
(ID "exit")
(PUNCT "(")
(LIT 1)
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD def)
(ID "testall")
(PUNCT "(")
(ID "list")
(PUNCT ",")
(ID "recursive")
(PUNCT ",")
(ID "toplevel")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD import)
(ID "sys")
(NEWLINE)
(KEYWORD import)
(ID "os")
(NEWLINE)
(KEYWORD for)
(ID "filename")
(KEYWORD in)
(ID "list")
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
(ID "filename")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "print")
(PUNCT "(")
(ID "filename")
(PUNCT "+")
(LIT "/:")
(PUNCT ",")
(ID "end")
(PUNCT "=")
(LIT " ")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(ID "recursive")
(KEYWORD or)
(ID "toplevel")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "print")
(PUNCT "(")
(LIT "recursing down:")
(PUNCT ")")
(NEWLINE)
(KEYWORD import)
(ID "glob")
(NEWLINE)
(ID "names")
(PUNCT "=")
(ID "glob")
(PUNCT ".")
(ID "glob")
(PUNCT "(")
(ID "os")
(PUNCT ".")
(ID "path")
(PUNCT ".")
(ID "join")
(PUNCT "(")
(ID "filename")
(PUNCT ",")
(LIT "*")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(ID "testall")
(PUNCT "(")
(ID "names")
(PUNCT ",")
(ID "recursive")
(PUNCT ",")
(LIT 0)
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "print")
(PUNCT "(")
(LIT "*** directory (use -r) ***")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "print")
(PUNCT "(")
(ID "filename")
(PUNCT "+")
(LIT ":")
(PUNCT ",")
(ID "end")
(PUNCT "=")
(LIT " ")
(PUNCT ")")
(NEWLINE)
(ID "sys")
(PUNCT ".")
(ID "stdout")
(PUNCT ".")
(ID "flush")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(KEYWORD try)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "print")
(PUNCT "(")
(ID "what")
(PUNCT "(")
(ID "filename")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD except)
(ID "OSError")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "print")
(PUNCT "(")
(LIT "*** not found ***")
(PUNCT ")")
(NEWLINE)
(DEDENT)
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
