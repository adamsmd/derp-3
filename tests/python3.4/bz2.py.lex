(LIT "Interface to the libbzip2 compression library.\n\nThis module provides a file interface, classes for incremental\n(de)compression, and functions for one-shot (de)compression.\n")
(NEWLINE)
(ID "__all__")
(PUNCT "=")
(PUNCT "[")
(LIT "BZ2File")
(PUNCT ",")
(LIT "BZ2Compressor")
(PUNCT ",")
(LIT "BZ2Decompressor")
(PUNCT ",")
(LIT "open")
(PUNCT ",")
(LIT "compress")
(PUNCT ",")
(LIT "decompress")
(PUNCT "]")
(NEWLINE)
(ID "__author__")
(PUNCT "=")
(LIT "Nadeem Vawda <nadeem.vawda@gmail.com>")
(NEWLINE)
(KEYWORD import)
(ID "io")
(NEWLINE)
(KEYWORD import)
(ID "warnings")
(NEWLINE)
(KEYWORD try)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD from)
(ID "threading")
(KEYWORD import)
(ID "RLock")
(NEWLINE)
(DEDENT)
(KEYWORD except)
(ID "ImportError")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD from)
(ID "dummy_threading")
(KEYWORD import)
(ID "RLock")
(NEWLINE)
(DEDENT)
(KEYWORD from)
(ID "_bz2")
(KEYWORD import)
(ID "BZ2Compressor")
(PUNCT ",")
(ID "BZ2Decompressor")
(NEWLINE)
(ID "_MODE_CLOSED")
(PUNCT "=")
(LIT 0)
(NEWLINE)
(ID "_MODE_READ")
(PUNCT "=")
(LIT 1)
(NEWLINE)
(ID "_MODE_READ_EOF")
(PUNCT "=")
(LIT 2)
(NEWLINE)
(ID "_MODE_WRITE")
(PUNCT "=")
(LIT 3)
(NEWLINE)
(ID "_BUFFER_SIZE")
(PUNCT "=")
(LIT 8192)
(NEWLINE)
(ID "_builtin_open")
(PUNCT "=")
(ID "open")
(NEWLINE)
(KEYWORD class)
(ID "BZ2File")
(PUNCT "(")
(ID "io")
(PUNCT ".")
(ID "BufferedIOBase")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "A file object providing transparent bzip2 (de)compression.\n\n    A BZ2File can act as a wrapper for an existing file object, or refer\n    directly to a named file on disk.\n\n    Note that BZ2File provides a *binary* file interface - data read is\n    returned as bytes, and data to be written should be given as bytes.\n    ")
(NEWLINE)
(KEYWORD def)
(ID "__init__")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "filename")
(PUNCT ",")
(ID "mode")
(PUNCT "=")
(LIT "r")
(PUNCT ",")
(ID "buffering")
(PUNCT "=")
(KEYWORD None)
(PUNCT ",")
(ID "compresslevel")
(PUNCT "=")
(LIT 9)
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Open a bzip2-compressed file.\n\n        If filename is a str or bytes object, it gives the name\n        of the file to be opened. Otherwise, it should be a file object,\n        which will be used to read or write the compressed data.\n\n        mode can be 'r' for reading (default), 'w' for (over)writing,\n        'x' for creating exclusively, or 'a' for appending. These can\n        equivalently be given as 'rb', 'wb', 'xb', and 'ab'.\n\n        buffering is ignored. Its use is deprecated.\n\n        If mode is 'w', 'x' or 'a', compresslevel can be a number between 1\n        and 9 specifying the level of compression: 1 produces the least\n        compression, and 9 (default) produces the most compression.\n\n        If mode is 'r', the input file may be the concatenation of\n        multiple compressed streams.\n        ")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_lock")
(PUNCT "=")
(ID "RLock")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_fp")
(PUNCT "=")
(KEYWORD None)
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_closefp")
(PUNCT "=")
(KEYWORD False)
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_mode")
(PUNCT "=")
(ID "_MODE_CLOSED")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_pos")
(PUNCT "=")
(LIT 0)
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_size")
(PUNCT "=")
(PUNCT "-")
(LIT 1)
(NEWLINE)
(KEYWORD if)
(ID "buffering")
(KEYWORD is)
(KEYWORD not)
(KEYWORD None)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "warnings")
(PUNCT ".")
(ID "warn")
(PUNCT "(")
(LIT "Use of 'buffering' argument is deprecated")
(PUNCT ",")
(ID "DeprecationWarning")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD if)
(KEYWORD not)
(PUNCT "(")
(LIT 1)
(PUNCT "<=")
(ID "compresslevel")
(PUNCT "<=")
(LIT 9)
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD raise)
(ID "ValueError")
(PUNCT "(")
(LIT "compresslevel must be between 1 and 9")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD if)
(ID "mode")
(KEYWORD in)
(PUNCT "(")
(LIT "")
(PUNCT ",")
(LIT "r")
(PUNCT ",")
(LIT "rb")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "mode")
(PUNCT "=")
(LIT "rb")
(NEWLINE)
(ID "mode_code")
(PUNCT "=")
(ID "_MODE_READ")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_decompressor")
(PUNCT "=")
(ID "BZ2Decompressor")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_buffer")
(PUNCT "=")
(LIT #"")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_buffer_offset")
(PUNCT "=")
(LIT 0)
(NEWLINE)
(DEDENT)
(KEYWORD elif)
(ID "mode")
(KEYWORD in)
(PUNCT "(")
(LIT "w")
(PUNCT ",")
(LIT "wb")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "mode")
(PUNCT "=")
(LIT "wb")
(NEWLINE)
(ID "mode_code")
(PUNCT "=")
(ID "_MODE_WRITE")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_compressor")
(PUNCT "=")
(ID "BZ2Compressor")
(PUNCT "(")
(ID "compresslevel")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD elif)
(ID "mode")
(KEYWORD in)
(PUNCT "(")
(LIT "x")
(PUNCT ",")
(LIT "xb")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "mode")
(PUNCT "=")
(LIT "xb")
(NEWLINE)
(ID "mode_code")
(PUNCT "=")
(ID "_MODE_WRITE")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_compressor")
(PUNCT "=")
(ID "BZ2Compressor")
(PUNCT "(")
(ID "compresslevel")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD elif)
(ID "mode")
(KEYWORD in)
(PUNCT "(")
(LIT "a")
(PUNCT ",")
(LIT "ab")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "mode")
(PUNCT "=")
(LIT "ab")
(NEWLINE)
(ID "mode_code")
(PUNCT "=")
(ID "_MODE_WRITE")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_compressor")
(PUNCT "=")
(ID "BZ2Compressor")
(PUNCT "(")
(ID "compresslevel")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD raise)
(ID "ValueError")
(PUNCT "(")
(LIT "Invalid mode: %r")
(PUNCT "%")
(PUNCT "(")
(ID "mode")
(PUNCT ",")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD if)
(ID "isinstance")
(PUNCT "(")
(ID "filename")
(PUNCT ",")
(PUNCT "(")
(ID "str")
(PUNCT ",")
(ID "bytes")
(PUNCT ")")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "_fp")
(PUNCT "=")
(ID "_builtin_open")
(PUNCT "(")
(ID "filename")
(PUNCT ",")
(ID "mode")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_closefp")
(PUNCT "=")
(KEYWORD True)
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_mode")
(PUNCT "=")
(ID "mode_code")
(NEWLINE)
(DEDENT)
(KEYWORD elif)
(ID "hasattr")
(PUNCT "(")
(ID "filename")
(PUNCT ",")
(LIT "read")
(PUNCT ")")
(KEYWORD or)
(ID "hasattr")
(PUNCT "(")
(ID "filename")
(PUNCT ",")
(LIT "write")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "_fp")
(PUNCT "=")
(ID "filename")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_mode")
(PUNCT "=")
(ID "mode_code")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD raise)
(ID "TypeError")
(PUNCT "(")
(LIT "filename must be a str or bytes object, or a file")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD def)
(ID "close")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Flush and close the file.\n\n        May be called more than once without error. Once the file is\n        closed, any other operation on it will raise a ValueError.\n        ")
(NEWLINE)
(KEYWORD with)
(ID "self")
(PUNCT ".")
(ID "_lock")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "self")
(PUNCT ".")
(ID "_mode")
(PUNCT "==")
(ID "_MODE_CLOSED")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(NEWLINE)
(DEDENT)
(KEYWORD try)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "self")
(PUNCT ".")
(ID "_mode")
(KEYWORD in)
(PUNCT "(")
(ID "_MODE_READ")
(PUNCT ",")
(ID "_MODE_READ_EOF")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "_decompressor")
(PUNCT "=")
(KEYWORD None)
(NEWLINE)
(DEDENT)
(KEYWORD elif)
(ID "self")
(PUNCT ".")
(ID "_mode")
(PUNCT "==")
(ID "_MODE_WRITE")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "_fp")
(PUNCT ".")
(ID "write")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "_compressor")
(PUNCT ".")
(ID "flush")
(PUNCT "(")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_compressor")
(PUNCT "=")
(KEYWORD None)
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
(KEYWORD if)
(ID "self")
(PUNCT ".")
(ID "_closefp")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "_fp")
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
(ID "self")
(PUNCT ".")
(ID "_fp")
(PUNCT "=")
(KEYWORD None)
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_closefp")
(PUNCT "=")
(KEYWORD False)
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_mode")
(PUNCT "=")
(ID "_MODE_CLOSED")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_buffer")
(PUNCT "=")
(LIT #"")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_buffer_offset")
(PUNCT "=")
(LIT 0)
(NEWLINE)
(DEDENT)
(DEDENT)
(DEDENT)
(DEDENT)
(PUNCT "@")
(ID "property")
(NEWLINE)
(KEYWORD def)
(ID "closed")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "True if this file is closed.")
(NEWLINE)
(KEYWORD return)
(ID "self")
(PUNCT ".")
(ID "_mode")
(PUNCT "==")
(ID "_MODE_CLOSED")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "fileno")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Return the file descriptor for the underlying file.")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_check_not_closed")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(KEYWORD return)
(ID "self")
(PUNCT ".")
(ID "_fp")
(PUNCT ".")
(ID "fileno")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "seekable")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Return whether the file supports seeking.")
(NEWLINE)
(KEYWORD return)
(ID "self")
(PUNCT ".")
(ID "readable")
(PUNCT "(")
(PUNCT ")")
(KEYWORD and)
(ID "self")
(PUNCT ".")
(ID "_fp")
(PUNCT ".")
(ID "seekable")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "readable")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Return whether the file was opened for reading.")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_check_not_closed")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(KEYWORD return)
(ID "self")
(PUNCT ".")
(ID "_mode")
(KEYWORD in)
(PUNCT "(")
(ID "_MODE_READ")
(PUNCT ",")
(ID "_MODE_READ_EOF")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "writable")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Return whether the file was opened for writing.")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_check_not_closed")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(KEYWORD return)
(ID "self")
(PUNCT ".")
(ID "_mode")
(PUNCT "==")
(ID "_MODE_WRITE")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "_check_not_closed")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "self")
(PUNCT ".")
(ID "closed")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD raise)
(ID "ValueError")
(PUNCT "(")
(LIT "I/O operation on closed file")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD def)
(ID "_check_can_read")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "self")
(PUNCT ".")
(ID "_mode")
(KEYWORD not)
(KEYWORD in)
(PUNCT "(")
(ID "_MODE_READ")
(PUNCT ",")
(ID "_MODE_READ_EOF")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "_check_not_closed")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(KEYWORD raise)
(ID "io")
(PUNCT ".")
(ID "UnsupportedOperation")
(PUNCT "(")
(LIT "File not open for reading")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD def)
(ID "_check_can_write")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "self")
(PUNCT ".")
(ID "_mode")
(PUNCT "!=")
(ID "_MODE_WRITE")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "_check_not_closed")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(KEYWORD raise)
(ID "io")
(PUNCT ".")
(ID "UnsupportedOperation")
(PUNCT "(")
(LIT "File not open for writing")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD def)
(ID "_check_can_seek")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "self")
(PUNCT ".")
(ID "_mode")
(KEYWORD not)
(KEYWORD in)
(PUNCT "(")
(ID "_MODE_READ")
(PUNCT ",")
(ID "_MODE_READ_EOF")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "_check_not_closed")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(KEYWORD raise)
(ID "io")
(PUNCT ".")
(ID "UnsupportedOperation")
(PUNCT "(")
(LIT "Seeking is only supported ")
(LIT "on files open for reading")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD if)
(KEYWORD not)
(ID "self")
(PUNCT ".")
(ID "_fp")
(PUNCT ".")
(ID "seekable")
(PUNCT "(")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD raise)
(ID "io")
(PUNCT ".")
(ID "UnsupportedOperation")
(PUNCT "(")
(LIT "The underlying file object ")
(LIT "does not support seeking")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD def)
(ID "_fill_buffer")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "self")
(PUNCT ".")
(ID "_mode")
(PUNCT "==")
(ID "_MODE_READ_EOF")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(KEYWORD False)
(NEWLINE)
(DEDENT)
(KEYWORD while)
(ID "self")
(PUNCT ".")
(ID "_buffer_offset")
(PUNCT "==")
(ID "len")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "_buffer")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "rawblock")
(PUNCT "=")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "_decompressor")
(PUNCT ".")
(ID "unused_data")
(KEYWORD or)
(ID "self")
(PUNCT ".")
(ID "_fp")
(PUNCT ".")
(ID "read")
(PUNCT "(")
(ID "_BUFFER_SIZE")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(KEYWORD not)
(ID "rawblock")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "self")
(PUNCT ".")
(ID "_decompressor")
(PUNCT ".")
(ID "eof")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "_mode")
(PUNCT "=")
(ID "_MODE_READ_EOF")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_size")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "_pos")
(NEWLINE)
(KEYWORD return)
(KEYWORD False)
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD raise)
(ID "EOFError")
(PUNCT "(")
(LIT "Compressed file ended before the ")
(LIT "end-of-stream marker was reached")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD if)
(ID "self")
(PUNCT ".")
(ID "_decompressor")
(PUNCT ".")
(ID "eof")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "_decompressor")
(PUNCT "=")
(ID "BZ2Decompressor")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(KEYWORD try)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "_buffer")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "_decompressor")
(PUNCT ".")
(ID "decompress")
(PUNCT "(")
(ID "rawblock")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD except)
(ID "OSError")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "_mode")
(PUNCT "=")
(ID "_MODE_READ_EOF")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_size")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "_pos")
(NEWLINE)
(KEYWORD return)
(KEYWORD False)
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "_buffer")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "_decompressor")
(PUNCT ".")
(ID "decompress")
(PUNCT "(")
(ID "rawblock")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "self")
(PUNCT ".")
(ID "_buffer_offset")
(PUNCT "=")
(LIT 0)
(NEWLINE)
(DEDENT)
(KEYWORD return)
(KEYWORD True)
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "_read_all")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "return_data")
(PUNCT "=")
(KEYWORD True)
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "_buffer")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "_buffer")
(PUNCT "[")
(ID "self")
(PUNCT ".")
(ID "_buffer_offset")
(PUNCT ":")
(PUNCT "]")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_buffer_offset")
(PUNCT "=")
(LIT 0)
(NEWLINE)
(ID "blocks")
(PUNCT "=")
(PUNCT "[")
(PUNCT "]")
(NEWLINE)
(KEYWORD while)
(ID "self")
(PUNCT ".")
(ID "_fill_buffer")
(PUNCT "(")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "return_data")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "blocks")
(PUNCT ".")
(ID "append")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "_buffer")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "self")
(PUNCT ".")
(ID "_pos")
(PUNCT "+=")
(ID "len")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "_buffer")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_buffer")
(PUNCT "=")
(LIT #"")
(NEWLINE)
(DEDENT)
(KEYWORD if)
(ID "return_data")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(LIT #"")
(PUNCT ".")
(ID "join")
(PUNCT "(")
(ID "blocks")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD def)
(ID "_read_block")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "n")
(PUNCT ",")
(ID "return_data")
(PUNCT "=")
(KEYWORD True)
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "end")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "_buffer_offset")
(PUNCT "+")
(ID "n")
(NEWLINE)
(KEYWORD if)
(ID "end")
(PUNCT "<=")
(ID "len")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "_buffer")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "data")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "_buffer")
(PUNCT "[")
(ID "self")
(PUNCT ".")
(ID "_buffer_offset")
(PUNCT ":")
(ID "end")
(PUNCT "]")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_buffer_offset")
(PUNCT "=")
(ID "end")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_pos")
(PUNCT "+=")
(ID "len")
(PUNCT "(")
(ID "data")
(PUNCT ")")
(NEWLINE)
(KEYWORD return)
(ID "data")
(KEYWORD if)
(ID "return_data")
(KEYWORD else)
(KEYWORD None)
(NEWLINE)
(DEDENT)
(ID "self")
(PUNCT ".")
(ID "_buffer")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "_buffer")
(PUNCT "[")
(ID "self")
(PUNCT ".")
(ID "_buffer_offset")
(PUNCT ":")
(PUNCT "]")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_buffer_offset")
(PUNCT "=")
(LIT 0)
(NEWLINE)
(ID "blocks")
(PUNCT "=")
(PUNCT "[")
(PUNCT "]")
(NEWLINE)
(KEYWORD while)
(ID "n")
(PUNCT ">")
(LIT 0)
(KEYWORD and)
(ID "self")
(PUNCT ".")
(ID "_fill_buffer")
(PUNCT "(")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "n")
(PUNCT "<")
(ID "len")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "_buffer")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "data")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "_buffer")
(PUNCT "[")
(PUNCT ":")
(ID "n")
(PUNCT "]")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_buffer_offset")
(PUNCT "=")
(ID "n")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "data")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "_buffer")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_buffer")
(PUNCT "=")
(LIT #"")
(NEWLINE)
(DEDENT)
(KEYWORD if)
(ID "return_data")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "blocks")
(PUNCT ".")
(ID "append")
(PUNCT "(")
(ID "data")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "self")
(PUNCT ".")
(ID "_pos")
(PUNCT "+=")
(ID "len")
(PUNCT "(")
(ID "data")
(PUNCT ")")
(NEWLINE)
(ID "n")
(PUNCT "-=")
(ID "len")
(PUNCT "(")
(ID "data")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD if)
(ID "return_data")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(LIT #"")
(PUNCT ".")
(ID "join")
(PUNCT "(")
(ID "blocks")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD def)
(ID "peek")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "n")
(PUNCT "=")
(LIT 0)
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Return buffered data without advancing the file position.\n\n        Always returns at least one byte of data, unless at EOF.\n        The exact number of bytes returned is unspecified.\n        ")
(NEWLINE)
(KEYWORD with)
(ID "self")
(PUNCT ".")
(ID "_lock")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "_check_can_read")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(KEYWORD not)
(ID "self")
(PUNCT ".")
(ID "_fill_buffer")
(PUNCT "(")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(LIT #"")
(NEWLINE)
(DEDENT)
(KEYWORD return)
(ID "self")
(PUNCT ".")
(ID "_buffer")
(PUNCT "[")
(ID "self")
(PUNCT ".")
(ID "_buffer_offset")
(PUNCT ":")
(PUNCT "]")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD def)
(ID "read")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "size")
(PUNCT "=")
(PUNCT "-")
(LIT 1)
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Read up to size uncompressed bytes from the file.\n\n        If size is negative or omitted, read until EOF is reached.\n        Returns b'' if the file is already at EOF.\n        ")
(NEWLINE)
(KEYWORD with)
(ID "self")
(PUNCT ".")
(ID "_lock")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "_check_can_read")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(ID "size")
(PUNCT "==")
(LIT 0)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(LIT #"")
(NEWLINE)
(DEDENT)
(KEYWORD elif)
(ID "size")
(PUNCT "<")
(LIT 0)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(ID "self")
(PUNCT ".")
(ID "_read_all")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(ID "self")
(PUNCT ".")
(ID "_read_block")
(PUNCT "(")
(ID "size")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(DEDENT)
(KEYWORD def)
(ID "read1")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "size")
(PUNCT "=")
(PUNCT "-")
(LIT 1)
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Read up to size uncompressed bytes, while trying to avoid\n        making multiple reads from the underlying stream.\n\n        Returns b'' if the file is at EOF.\n        ")
(NEWLINE)
(KEYWORD with)
(ID "self")
(PUNCT ".")
(ID "_lock")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "_check_can_read")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(PUNCT "(")
(ID "size")
(PUNCT "==")
(LIT 0)
(KEYWORD or)
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "_buffer_offset")
(PUNCT "==")
(ID "len")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "_buffer")
(PUNCT ")")
(KEYWORD and)
(KEYWORD not)
(ID "self")
(PUNCT ".")
(ID "_fill_buffer")
(PUNCT "(")
(PUNCT ")")
(PUNCT ")")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(LIT #"")
(NEWLINE)
(DEDENT)
(KEYWORD if)
(ID "size")
(PUNCT ">")
(LIT 0)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "data")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "_buffer")
(PUNCT "[")
(ID "self")
(PUNCT ".")
(ID "_buffer_offset")
(PUNCT ":")
(ID "self")
(PUNCT ".")
(ID "_buffer_offset")
(PUNCT "+")
(ID "size")
(PUNCT "]")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_buffer_offset")
(PUNCT "+=")
(ID "len")
(PUNCT "(")
(ID "data")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "data")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "_buffer")
(PUNCT "[")
(ID "self")
(PUNCT ".")
(ID "_buffer_offset")
(PUNCT ":")
(PUNCT "]")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_buffer")
(PUNCT "=")
(LIT #"")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_buffer_offset")
(PUNCT "=")
(LIT 0)
(NEWLINE)
(DEDENT)
(ID "self")
(PUNCT ".")
(ID "_pos")
(PUNCT "+=")
(ID "len")
(PUNCT "(")
(ID "data")
(PUNCT ")")
(NEWLINE)
(KEYWORD return)
(ID "data")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD def)
(ID "readinto")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "b")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Read up to len(b) bytes into b.\n\n        Returns the number of bytes read (0 for EOF).\n        ")
(NEWLINE)
(KEYWORD with)
(ID "self")
(PUNCT ".")
(ID "_lock")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(ID "io")
(PUNCT ".")
(ID "BufferedIOBase")
(PUNCT ".")
(ID "readinto")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "b")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD def)
(ID "readline")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "size")
(PUNCT "=")
(PUNCT "-")
(LIT 1)
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Read a line of uncompressed bytes from the file.\n\n        The terminating newline (if present) is retained. If size is\n        non-negative, no more than size bytes will be read (in which\n        case the line may be incomplete). Returns b'' if already at EOF.\n        ")
(NEWLINE)
(KEYWORD if)
(KEYWORD not)
(ID "isinstance")
(PUNCT "(")
(ID "size")
(PUNCT ",")
(ID "int")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(KEYWORD not)
(ID "hasattr")
(PUNCT "(")
(ID "size")
(PUNCT ",")
(LIT "__index__")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD raise)
(ID "TypeError")
(PUNCT "(")
(LIT "Integer argument expected")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "size")
(PUNCT "=")
(ID "size")
(PUNCT ".")
(ID "__index__")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD with)
(ID "self")
(PUNCT ".")
(ID "_lock")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "_check_can_read")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(ID "size")
(PUNCT "<")
(LIT 0)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "end")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "_buffer")
(PUNCT ".")
(ID "find")
(PUNCT "(")
(LIT #"\n")
(PUNCT ",")
(ID "self")
(PUNCT ".")
(ID "_buffer_offset")
(PUNCT ")")
(PUNCT "+")
(LIT 1)
(NEWLINE)
(KEYWORD if)
(ID "end")
(PUNCT ">")
(LIT 0)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "line")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "_buffer")
(PUNCT "[")
(ID "self")
(PUNCT ".")
(ID "_buffer_offset")
(PUNCT ":")
(ID "end")
(PUNCT "]")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_buffer_offset")
(PUNCT "=")
(ID "end")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_pos")
(PUNCT "+=")
(ID "len")
(PUNCT "(")
(ID "line")
(PUNCT ")")
(NEWLINE)
(KEYWORD return)
(ID "line")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD return)
(ID "io")
(PUNCT ".")
(ID "BufferedIOBase")
(PUNCT ".")
(ID "readline")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "size")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD def)
(ID "readlines")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "size")
(PUNCT "=")
(PUNCT "-")
(LIT 1)
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Read a list of lines of uncompressed bytes from the file.\n\n        size can be specified to control the number of lines read: no\n        further lines will be read once the total size of the lines read\n        so far equals or exceeds size.\n        ")
(NEWLINE)
(KEYWORD if)
(KEYWORD not)
(ID "isinstance")
(PUNCT "(")
(ID "size")
(PUNCT ",")
(ID "int")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(KEYWORD not)
(ID "hasattr")
(PUNCT "(")
(ID "size")
(PUNCT ",")
(LIT "__index__")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD raise)
(ID "TypeError")
(PUNCT "(")
(LIT "Integer argument expected")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "size")
(PUNCT "=")
(ID "size")
(PUNCT ".")
(ID "__index__")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD with)
(ID "self")
(PUNCT ".")
(ID "_lock")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(ID "io")
(PUNCT ".")
(ID "BufferedIOBase")
(PUNCT ".")
(ID "readlines")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "size")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD def)
(ID "write")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "data")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Write a byte string to the file.\n\n        Returns the number of uncompressed bytes written, which is\n        always len(data). Note that due to buffering, the file on disk\n        may not reflect the data written until close() is called.\n        ")
(NEWLINE)
(KEYWORD with)
(ID "self")
(PUNCT ".")
(ID "_lock")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "_check_can_write")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "compressed")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "_compressor")
(PUNCT ".")
(ID "compress")
(PUNCT "(")
(ID "data")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_fp")
(PUNCT ".")
(ID "write")
(PUNCT "(")
(ID "compressed")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_pos")
(PUNCT "+=")
(ID "len")
(PUNCT "(")
(ID "data")
(PUNCT ")")
(NEWLINE)
(KEYWORD return)
(ID "len")
(PUNCT "(")
(ID "data")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD def)
(ID "writelines")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "seq")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Write a sequence of byte strings to the file.\n\n        Returns the number of uncompressed bytes written.\n        seq can be any iterable yielding byte strings.\n\n        Line separators are not added between the written byte strings.\n        ")
(NEWLINE)
(KEYWORD with)
(ID "self")
(PUNCT ".")
(ID "_lock")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(ID "io")
(PUNCT ".")
(ID "BufferedIOBase")
(PUNCT ".")
(ID "writelines")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "seq")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD def)
(ID "_rewind")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "_fp")
(PUNCT ".")
(ID "seek")
(PUNCT "(")
(LIT 0)
(PUNCT ",")
(LIT 0)
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_mode")
(PUNCT "=")
(ID "_MODE_READ")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_pos")
(PUNCT "=")
(LIT 0)
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_decompressor")
(PUNCT "=")
(ID "BZ2Decompressor")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_buffer")
(PUNCT "=")
(LIT #"")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_buffer_offset")
(PUNCT "=")
(LIT 0)
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "seek")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "offset")
(PUNCT ",")
(ID "whence")
(PUNCT "=")
(LIT 0)
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Change the file position.\n\n        The new position is specified by offset, relative to the\n        position indicated by whence. Values for whence are:\n\n            0: start of stream (default); offset must not be negative\n            1: current stream position\n            2: end of stream; offset must not be positive\n\n        Returns the new file position.\n\n        Note that seeking is emulated, so depending on the parameters,\n        this operation may be extremely slow.\n        ")
(NEWLINE)
(KEYWORD with)
(ID "self")
(PUNCT ".")
(ID "_lock")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "_check_can_seek")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(ID "whence")
(PUNCT "==")
(LIT 0)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD pass)
(NEWLINE)
(DEDENT)
(KEYWORD elif)
(ID "whence")
(PUNCT "==")
(LIT 1)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "offset")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "_pos")
(PUNCT "+")
(ID "offset")
(NEWLINE)
(DEDENT)
(KEYWORD elif)
(ID "whence")
(PUNCT "==")
(LIT 2)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "self")
(PUNCT ".")
(ID "_size")
(PUNCT "<")
(LIT 0)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "_read_all")
(PUNCT "(")
(ID "return_data")
(PUNCT "=")
(KEYWORD False)
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "offset")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "_size")
(PUNCT "+")
(ID "offset")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD raise)
(ID "ValueError")
(PUNCT "(")
(LIT "Invalid value for whence: %s")
(PUNCT "%")
(PUNCT "(")
(ID "whence")
(PUNCT ",")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD if)
(ID "offset")
(PUNCT "<")
(ID "self")
(PUNCT ".")
(ID "_pos")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "_rewind")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "offset")
(PUNCT "-=")
(ID "self")
(PUNCT ".")
(ID "_pos")
(NEWLINE)
(DEDENT)
(ID "self")
(PUNCT ".")
(ID "_read_block")
(PUNCT "(")
(ID "offset")
(PUNCT ",")
(ID "return_data")
(PUNCT "=")
(KEYWORD False)
(PUNCT ")")
(NEWLINE)
(KEYWORD return)
(ID "self")
(PUNCT ".")
(ID "_pos")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD def)
(ID "tell")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Return the current file position.")
(NEWLINE)
(KEYWORD with)
(ID "self")
(PUNCT ".")
(ID "_lock")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "_check_not_closed")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(KEYWORD return)
(ID "self")
(PUNCT ".")
(ID "_pos")
(NEWLINE)
(DEDENT)
(DEDENT)
(DEDENT)
(KEYWORD def)
(ID "open")
(PUNCT "(")
(ID "filename")
(PUNCT ",")
(ID "mode")
(PUNCT "=")
(LIT "rb")
(PUNCT ",")
(ID "compresslevel")
(PUNCT "=")
(LIT 9)
(PUNCT ",")
(ID "encoding")
(PUNCT "=")
(KEYWORD None)
(PUNCT ",")
(ID "errors")
(PUNCT "=")
(KEYWORD None)
(PUNCT ",")
(ID "newline")
(PUNCT "=")
(KEYWORD None)
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Open a bzip2-compressed file in binary or text mode.\n\n    The filename argument can be an actual filename (a str or bytes\n    object), or an existing file object to read from or write to.\n\n    The mode argument can be \"r\", \"rb\", \"w\", \"wb\", \"x\", \"xb\", \"a\" or\n    \"ab\" for binary mode, or \"rt\", \"wt\", \"xt\" or \"at\" for text mode.\n    The default mode is \"rb\", and the default compresslevel is 9.\n\n    For binary mode, this function is equivalent to the BZ2File\n    constructor: BZ2File(filename, mode, compresslevel). In this case,\n    the encoding, errors and newline arguments must not be provided.\n\n    For text mode, a BZ2File object is created, and wrapped in an\n    io.TextIOWrapper instance with the specified encoding, error\n    handling behavior, and line ending(s).\n\n    ")
(NEWLINE)
(KEYWORD if)
(LIT "t")
(KEYWORD in)
(ID "mode")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(LIT "b")
(KEYWORD in)
(ID "mode")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD raise)
(ID "ValueError")
(PUNCT "(")
(LIT "Invalid mode: %r")
(PUNCT "%")
(PUNCT "(")
(ID "mode")
(PUNCT ",")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "encoding")
(KEYWORD is)
(KEYWORD not)
(KEYWORD None)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD raise)
(ID "ValueError")
(PUNCT "(")
(LIT "Argument 'encoding' not supported in binary mode")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD if)
(ID "errors")
(KEYWORD is)
(KEYWORD not)
(KEYWORD None)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD raise)
(ID "ValueError")
(PUNCT "(")
(LIT "Argument 'errors' not supported in binary mode")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD if)
(ID "newline")
(KEYWORD is)
(KEYWORD not)
(KEYWORD None)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD raise)
(ID "ValueError")
(PUNCT "(")
(LIT "Argument 'newline' not supported in binary mode")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(ID "bz_mode")
(PUNCT "=")
(ID "mode")
(PUNCT ".")
(ID "replace")
(PUNCT "(")
(LIT "t")
(PUNCT ",")
(LIT "")
(PUNCT ")")
(NEWLINE)
(ID "binary_file")
(PUNCT "=")
(ID "BZ2File")
(PUNCT "(")
(ID "filename")
(PUNCT ",")
(ID "bz_mode")
(PUNCT ",")
(ID "compresslevel")
(PUNCT "=")
(ID "compresslevel")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(LIT "t")
(KEYWORD in)
(ID "mode")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(ID "io")
(PUNCT ".")
(ID "TextIOWrapper")
(PUNCT "(")
(ID "binary_file")
(PUNCT ",")
(ID "encoding")
(PUNCT ",")
(ID "errors")
(PUNCT ",")
(ID "newline")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(ID "binary_file")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD def)
(ID "compress")
(PUNCT "(")
(ID "data")
(PUNCT ",")
(ID "compresslevel")
(PUNCT "=")
(LIT 9)
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Compress a block of data.\n\n    compresslevel, if given, must be a number between 1 and 9.\n\n    For incremental compression, use a BZ2Compressor object instead.\n    ")
(NEWLINE)
(ID "comp")
(PUNCT "=")
(ID "BZ2Compressor")
(PUNCT "(")
(ID "compresslevel")
(PUNCT ")")
(NEWLINE)
(KEYWORD return)
(ID "comp")
(PUNCT ".")
(ID "compress")
(PUNCT "(")
(ID "data")
(PUNCT ")")
(PUNCT "+")
(ID "comp")
(PUNCT ".")
(ID "flush")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "decompress")
(PUNCT "(")
(ID "data")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Decompress a block of data.\n\n    For incremental decompression, use a BZ2Decompressor object instead.\n    ")
(NEWLINE)
(ID "results")
(PUNCT "=")
(PUNCT "[")
(PUNCT "]")
(NEWLINE)
(KEYWORD while)
(ID "data")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "decomp")
(PUNCT "=")
(ID "BZ2Decompressor")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(KEYWORD try)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "res")
(PUNCT "=")
(ID "decomp")
(PUNCT ".")
(ID "decompress")
(PUNCT "(")
(ID "data")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD except)
(ID "OSError")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "results")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD break)
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD raise)
(NEWLINE)
(DEDENT)
(DEDENT)
(ID "results")
(PUNCT ".")
(ID "append")
(PUNCT "(")
(ID "res")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(KEYWORD not)
(ID "decomp")
(PUNCT ".")
(ID "eof")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD raise)
(ID "ValueError")
(PUNCT "(")
(LIT "Compressed data ended before the ")
(LIT "end-of-stream marker was reached")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "data")
(PUNCT "=")
(ID "decomp")
(PUNCT ".")
(ID "unused_data")
(NEWLINE)
(DEDENT)
(KEYWORD return)
(LIT #"")
(PUNCT ".")
(ID "join")
(PUNCT "(")
(ID "results")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ENDMARKER)