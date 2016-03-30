(LIT "email package exception classes.")
(NEWLINE)
(KEYWORD class)
(ID "MessageError")
(PUNCT "(")
(ID "Exception")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Base class for errors in the email package.")
(NEWLINE)
(DEDENT)
(KEYWORD class)
(ID "MessageParseError")
(PUNCT "(")
(ID "MessageError")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Base class for message parsing errors.")
(NEWLINE)
(DEDENT)
(KEYWORD class)
(ID "HeaderParseError")
(PUNCT "(")
(ID "MessageParseError")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Error while parsing headers.")
(NEWLINE)
(DEDENT)
(KEYWORD class)
(ID "BoundaryError")
(PUNCT "(")
(ID "MessageParseError")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Couldn't find terminating boundary.")
(NEWLINE)
(DEDENT)
(KEYWORD class)
(ID "MultipartConversionError")
(PUNCT "(")
(ID "MessageError")
(PUNCT ",")
(ID "TypeError")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Conversion to a multipart is prohibited.")
(NEWLINE)
(DEDENT)
(KEYWORD class)
(ID "CharsetError")
(PUNCT "(")
(ID "MessageError")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "An illegal charset was given.")
(NEWLINE)
(DEDENT)
(KEYWORD class)
(ID "MessageDefect")
(PUNCT "(")
(ID "ValueError")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Base class for a message defect.")
(NEWLINE)
(KEYWORD def)
(ID "__init__")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "line")
(PUNCT "=")
(KEYWORD None)
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "line")
(KEYWORD is)
(KEYWORD not)
(KEYWORD None)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "super")
(PUNCT "(")
(PUNCT ")")
(PUNCT ".")
(ID "__init__")
(PUNCT "(")
(ID "line")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "self")
(PUNCT ".")
(ID "line")
(PUNCT "=")
(ID "line")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD class)
(ID "NoBoundaryInMultipartDefect")
(PUNCT "(")
(ID "MessageDefect")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "A message claimed to be a multipart but had no boundary parameter.")
(NEWLINE)
(DEDENT)
(KEYWORD class)
(ID "StartBoundaryNotFoundDefect")
(PUNCT "(")
(ID "MessageDefect")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "The claimed start boundary was never found.")
(NEWLINE)
(DEDENT)
(KEYWORD class)
(ID "CloseBoundaryNotFoundDefect")
(PUNCT "(")
(ID "MessageDefect")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "A start boundary was found, but not the corresponding close boundary.")
(NEWLINE)
(DEDENT)
(KEYWORD class)
(ID "FirstHeaderLineIsContinuationDefect")
(PUNCT "(")
(ID "MessageDefect")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "A message had a continuation line as its first header line.")
(NEWLINE)
(DEDENT)
(KEYWORD class)
(ID "MisplacedEnvelopeHeaderDefect")
(PUNCT "(")
(ID "MessageDefect")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "A 'Unix-from' header was found in the middle of a header block.")
(NEWLINE)
(DEDENT)
(KEYWORD class)
(ID "MissingHeaderBodySeparatorDefect")
(PUNCT "(")
(ID "MessageDefect")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Found line with no leading whitespace and no colon before blank line.")
(NEWLINE)
(DEDENT)
(ID "MalformedHeaderDefect")
(PUNCT "=")
(ID "MissingHeaderBodySeparatorDefect")
(NEWLINE)
(KEYWORD class)
(ID "MultipartInvariantViolationDefect")
(PUNCT "(")
(ID "MessageDefect")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "A message claimed to be a multipart but no subparts were found.")
(NEWLINE)
(DEDENT)
(KEYWORD class)
(ID "InvalidMultipartContentTransferEncodingDefect")
(PUNCT "(")
(ID "MessageDefect")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "An invalid content transfer encoding was set on the multipart itself.")
(NEWLINE)
(DEDENT)
(KEYWORD class)
(ID "UndecodableBytesDefect")
(PUNCT "(")
(ID "MessageDefect")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Header contained bytes that could not be decoded")
(NEWLINE)
(DEDENT)
(KEYWORD class)
(ID "InvalidBase64PaddingDefect")
(PUNCT "(")
(ID "MessageDefect")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "base64 encoded sequence had an incorrect length")
(NEWLINE)
(DEDENT)
(KEYWORD class)
(ID "InvalidBase64CharactersDefect")
(PUNCT "(")
(ID "MessageDefect")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "base64 encoded sequence had characters not in base64 alphabet")
(NEWLINE)
(DEDENT)
(KEYWORD class)
(ID "HeaderDefect")
(PUNCT "(")
(ID "MessageDefect")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Base class for a header defect.")
(NEWLINE)
(KEYWORD def)
(ID "__init__")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(PUNCT "*")
(ID "args")
(PUNCT ",")
(PUNCT "**")
(ID "kw")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "super")
(PUNCT "(")
(PUNCT ")")
(PUNCT ".")
(ID "__init__")
(PUNCT "(")
(PUNCT "*")
(ID "args")
(PUNCT ",")
(PUNCT "**")
(ID "kw")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD class)
(ID "InvalidHeaderDefect")
(PUNCT "(")
(ID "HeaderDefect")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Header is not valid, message gives details.")
(NEWLINE)
(DEDENT)
(KEYWORD class)
(ID "HeaderMissingRequiredValue")
(PUNCT "(")
(ID "HeaderDefect")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "A header that must have a value had none")
(NEWLINE)
(DEDENT)
(KEYWORD class)
(ID "NonPrintableDefect")
(PUNCT "(")
(ID "HeaderDefect")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "ASCII characters outside the ascii-printable range found")
(NEWLINE)
(KEYWORD def)
(ID "__init__")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "non_printables")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "super")
(PUNCT "(")
(PUNCT ")")
(PUNCT ".")
(ID "__init__")
(PUNCT "(")
(ID "non_printables")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "non_printables")
(PUNCT "=")
(ID "non_printables")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "__str__")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(PUNCT "(")
(LIT "the following ASCII non-printables found in header: ")
(LIT "{}")
(PUNCT ".")
(ID "format")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "non_printables")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD class)
(ID "ObsoleteHeaderDefect")
(PUNCT "(")
(ID "HeaderDefect")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Header uses syntax declared obsolete by RFC 5322")
(NEWLINE)
(DEDENT)
(KEYWORD class)
(ID "NonASCIILocalPartDefect")
(PUNCT "(")
(ID "HeaderDefect")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "local_part contains non-ASCII characters")
(NEWLINE)
(DEDENT)
(ENDMARKER)