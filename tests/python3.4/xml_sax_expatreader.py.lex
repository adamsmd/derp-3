(LIT "\nSAX driver for the pyexpat C module.  This driver works with\npyexpat.__version__ == '2.22'.\n")
(NEWLINE)
(ID "version")
(PUNCT "=")
(LIT "0.20")
(NEWLINE)
(KEYWORD from)
(ID "xml")
(PUNCT ".")
(ID "sax")
(PUNCT ".")
(ID "_exceptions")
(KEYWORD import)
(PUNCT "*")
(NEWLINE)
(KEYWORD from)
(ID "xml")
(PUNCT ".")
(ID "sax")
(PUNCT ".")
(ID "handler")
(KEYWORD import)
(ID "feature_validation")
(PUNCT ",")
(ID "feature_namespaces")
(NEWLINE)
(KEYWORD from)
(ID "xml")
(PUNCT ".")
(ID "sax")
(PUNCT ".")
(ID "handler")
(KEYWORD import)
(ID "feature_namespace_prefixes")
(NEWLINE)
(KEYWORD from)
(ID "xml")
(PUNCT ".")
(ID "sax")
(PUNCT ".")
(ID "handler")
(KEYWORD import)
(ID "feature_external_ges")
(PUNCT ",")
(ID "feature_external_pes")
(NEWLINE)
(KEYWORD from)
(ID "xml")
(PUNCT ".")
(ID "sax")
(PUNCT ".")
(ID "handler")
(KEYWORD import)
(ID "feature_string_interning")
(NEWLINE)
(KEYWORD from)
(ID "xml")
(PUNCT ".")
(ID "sax")
(PUNCT ".")
(ID "handler")
(KEYWORD import)
(ID "property_xml_string")
(PUNCT ",")
(ID "property_interning_dict")
(NEWLINE)
(KEYWORD import)
(ID "sys")
(NEWLINE)
(KEYWORD if)
(ID "sys")
(PUNCT ".")
(ID "platform")
(PUNCT "[")
(PUNCT ":")
(LIT 4)
(PUNCT "]")
(PUNCT "==")
(LIT "java")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD raise)
(ID "SAXReaderNotAvailable")
(PUNCT "(")
(LIT "expat not available in Java")
(PUNCT ",")
(KEYWORD None)
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD del)
(ID "sys")
(NEWLINE)
(KEYWORD try)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD from)
(ID "xml")
(PUNCT ".")
(ID "parsers")
(KEYWORD import)
(ID "expat")
(NEWLINE)
(DEDENT)
(KEYWORD except)
(ID "ImportError")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD raise)
(ID "SAXReaderNotAvailable")
(PUNCT "(")
(LIT "expat not supported")
(PUNCT ",")
(KEYWORD None)
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(KEYWORD not)
(ID "hasattr")
(PUNCT "(")
(ID "expat")
(PUNCT ",")
(LIT "ParserCreate")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD raise)
(ID "SAXReaderNotAvailable")
(PUNCT "(")
(LIT "expat not supported")
(PUNCT ",")
(KEYWORD None)
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD from)
(ID "xml")
(PUNCT ".")
(ID "sax")
(KEYWORD import)
(ID "xmlreader")
(PUNCT ",")
(ID "saxutils")
(PUNCT ",")
(ID "handler")
(NEWLINE)
(ID "AttributesImpl")
(PUNCT "=")
(ID "xmlreader")
(PUNCT ".")
(ID "AttributesImpl")
(NEWLINE)
(ID "AttributesNSImpl")
(PUNCT "=")
(ID "xmlreader")
(PUNCT ".")
(ID "AttributesNSImpl")
(NEWLINE)
(KEYWORD try)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD import)
(ID "_weakref")
(NEWLINE)
(DEDENT)
(KEYWORD except)
(ID "ImportError")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD def)
(ID "_mkproxy")
(PUNCT "(")
(ID "o")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(ID "o")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD import)
(ID "weakref")
(NEWLINE)
(ID "_mkproxy")
(PUNCT "=")
(ID "weakref")
(PUNCT ".")
(ID "proxy")
(NEWLINE)
(KEYWORD del)
(ID "weakref")
(PUNCT ",")
(ID "_weakref")
(NEWLINE)
(DEDENT)
(KEYWORD class)
(ID "ExpatLocator")
(PUNCT "(")
(ID "xmlreader")
(PUNCT ".")
(ID "Locator")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Locator for use with the ExpatParser class.\n\n    This uses a weak reference to the parser object to avoid creating\n    a circular reference between the parser and the content handler.\n    ")
(NEWLINE)
(KEYWORD def)
(ID "__init__")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "parser")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "_ref")
(PUNCT "=")
(ID "_mkproxy")
(PUNCT "(")
(ID "parser")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "getColumnNumber")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "parser")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "_ref")
(NEWLINE)
(KEYWORD if)
(ID "parser")
(PUNCT ".")
(ID "_parser")
(KEYWORD is)
(KEYWORD None)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(KEYWORD None)
(NEWLINE)
(DEDENT)
(KEYWORD return)
(ID "parser")
(PUNCT ".")
(ID "_parser")
(PUNCT ".")
(ID "ErrorColumnNumber")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "getLineNumber")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "parser")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "_ref")
(NEWLINE)
(KEYWORD if)
(ID "parser")
(PUNCT ".")
(ID "_parser")
(KEYWORD is)
(KEYWORD None)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(LIT 1)
(NEWLINE)
(DEDENT)
(KEYWORD return)
(ID "parser")
(PUNCT ".")
(ID "_parser")
(PUNCT ".")
(ID "ErrorLineNumber")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "getPublicId")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "parser")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "_ref")
(NEWLINE)
(KEYWORD if)
(ID "parser")
(KEYWORD is)
(KEYWORD None)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(KEYWORD None)
(NEWLINE)
(DEDENT)
(KEYWORD return)
(ID "parser")
(PUNCT ".")
(ID "_source")
(PUNCT ".")
(ID "getPublicId")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "getSystemId")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "parser")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "_ref")
(NEWLINE)
(KEYWORD if)
(ID "parser")
(KEYWORD is)
(KEYWORD None)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(KEYWORD None)
(NEWLINE)
(DEDENT)
(KEYWORD return)
(ID "parser")
(PUNCT ".")
(ID "_source")
(PUNCT ".")
(ID "getSystemId")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD class)
(ID "ExpatParser")
(PUNCT "(")
(ID "xmlreader")
(PUNCT ".")
(ID "IncrementalParser")
(PUNCT ",")
(ID "xmlreader")
(PUNCT ".")
(ID "Locator")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "SAX driver for the pyexpat C module.")
(NEWLINE)
(KEYWORD def)
(ID "__init__")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "namespaceHandling")
(PUNCT "=")
(LIT 0)
(PUNCT ",")
(ID "bufsize")
(PUNCT "=")
(LIT 2)
(PUNCT "**")
(LIT 16)
(PUNCT "-")
(LIT 20)
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "xmlreader")
(PUNCT ".")
(ID "IncrementalParser")
(PUNCT ".")
(ID "__init__")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "bufsize")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_source")
(PUNCT "=")
(ID "xmlreader")
(PUNCT ".")
(ID "InputSource")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_parser")
(PUNCT "=")
(KEYWORD None)
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_namespaces")
(PUNCT "=")
(ID "namespaceHandling")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_lex_handler_prop")
(PUNCT "=")
(KEYWORD None)
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_parsing")
(PUNCT "=")
(LIT 0)
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_entity_stack")
(PUNCT "=")
(PUNCT "[")
(PUNCT "]")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_external_ges")
(PUNCT "=")
(LIT 1)
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_interning")
(PUNCT "=")
(KEYWORD None)
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "parse")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "source")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Parse an XML document from a URL or an InputSource.")
(NEWLINE)
(ID "source")
(PUNCT "=")
(ID "saxutils")
(PUNCT ".")
(ID "prepare_input_source")
(PUNCT "(")
(ID "source")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_source")
(PUNCT "=")
(ID "source")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "reset")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_cont_handler")
(PUNCT ".")
(ID "setDocumentLocator")
(PUNCT "(")
(ID "ExpatLocator")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(ID "xmlreader")
(PUNCT ".")
(ID "IncrementalParser")
(PUNCT ".")
(ID "parse")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "source")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "prepareParser")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "source")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "source")
(PUNCT ".")
(ID "getSystemId")
(PUNCT "(")
(PUNCT ")")
(KEYWORD is)
(KEYWORD not)
(KEYWORD None)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "_parser")
(PUNCT ".")
(ID "SetBase")
(PUNCT "(")
(ID "source")
(PUNCT ".")
(ID "getSystemId")
(PUNCT "(")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD def)
(ID "setContentHandler")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "handler")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "xmlreader")
(PUNCT ".")
(ID "IncrementalParser")
(PUNCT ".")
(ID "setContentHandler")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "handler")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(ID "self")
(PUNCT ".")
(ID "_parsing")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "_reset_cont_handler")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD def)
(ID "getFeature")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "name")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "name")
(PUNCT "==")
(ID "feature_namespaces")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(ID "self")
(PUNCT ".")
(ID "_namespaces")
(NEWLINE)
(DEDENT)
(KEYWORD elif)
(ID "name")
(PUNCT "==")
(ID "feature_string_interning")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(ID "self")
(PUNCT ".")
(ID "_interning")
(KEYWORD is)
(KEYWORD not)
(KEYWORD None)
(NEWLINE)
(DEDENT)
(KEYWORD elif)
(ID "name")
(KEYWORD in)
(PUNCT "(")
(ID "feature_validation")
(PUNCT ",")
(ID "feature_external_pes")
(PUNCT ",")
(ID "feature_namespace_prefixes")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(LIT 0)
(NEWLINE)
(DEDENT)
(KEYWORD elif)
(ID "name")
(PUNCT "==")
(ID "feature_external_ges")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(ID "self")
(PUNCT ".")
(ID "_external_ges")
(NEWLINE)
(DEDENT)
(KEYWORD raise)
(ID "SAXNotRecognizedException")
(PUNCT "(")
(LIT "Feature '%s' not recognized")
(PUNCT "%")
(ID "name")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "setFeature")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "name")
(PUNCT ",")
(ID "state")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "self")
(PUNCT ".")
(ID "_parsing")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD raise)
(ID "SAXNotSupportedException")
(PUNCT "(")
(LIT "Cannot set features while parsing")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD if)
(ID "name")
(PUNCT "==")
(ID "feature_namespaces")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "_namespaces")
(PUNCT "=")
(ID "state")
(NEWLINE)
(DEDENT)
(KEYWORD elif)
(ID "name")
(PUNCT "==")
(ID "feature_external_ges")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "_external_ges")
(PUNCT "=")
(ID "state")
(NEWLINE)
(DEDENT)
(KEYWORD elif)
(ID "name")
(PUNCT "==")
(ID "feature_string_interning")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "state")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "self")
(PUNCT ".")
(ID "_interning")
(KEYWORD is)
(KEYWORD None)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "_interning")
(PUNCT "=")
(PUNCT "{")
(PUNCT "}")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "_interning")
(PUNCT "=")
(KEYWORD None)
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD elif)
(ID "name")
(PUNCT "==")
(ID "feature_validation")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "state")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD raise)
(ID "SAXNotSupportedException")
(PUNCT "(")
(LIT "expat does not support validation")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD elif)
(ID "name")
(PUNCT "==")
(ID "feature_external_pes")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "state")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD raise)
(ID "SAXNotSupportedException")
(PUNCT "(")
(LIT "expat does not read external parameter entities")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD elif)
(ID "name")
(PUNCT "==")
(ID "feature_namespace_prefixes")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "state")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD raise)
(ID "SAXNotSupportedException")
(PUNCT "(")
(LIT "expat does not report namespace prefixes")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD raise)
(ID "SAXNotRecognizedException")
(PUNCT "(")
(LIT "Feature '%s' not recognized")
(PUNCT "%")
(ID "name")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD def)
(ID "getProperty")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "name")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "name")
(PUNCT "==")
(ID "handler")
(PUNCT ".")
(ID "property_lexical_handler")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(ID "self")
(PUNCT ".")
(ID "_lex_handler_prop")
(NEWLINE)
(DEDENT)
(KEYWORD elif)
(ID "name")
(PUNCT "==")
(ID "property_interning_dict")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(ID "self")
(PUNCT ".")
(ID "_interning")
(NEWLINE)
(DEDENT)
(KEYWORD elif)
(ID "name")
(PUNCT "==")
(ID "property_xml_string")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "self")
(PUNCT ".")
(ID "_parser")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "hasattr")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "_parser")
(PUNCT ",")
(LIT "GetInputContext")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(ID "self")
(PUNCT ".")
(ID "_parser")
(PUNCT ".")
(ID "GetInputContext")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD raise)
(ID "SAXNotRecognizedException")
(PUNCT "(")
(LIT "This version of expat does not support getting")
(LIT " the XML string")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD raise)
(ID "SAXNotSupportedException")
(PUNCT "(")
(LIT "XML string cannot be returned when not parsing")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD raise)
(ID "SAXNotRecognizedException")
(PUNCT "(")
(LIT "Property '%s' not recognized")
(PUNCT "%")
(ID "name")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "setProperty")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "name")
(PUNCT ",")
(ID "value")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "name")
(PUNCT "==")
(ID "handler")
(PUNCT ".")
(ID "property_lexical_handler")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "_lex_handler_prop")
(PUNCT "=")
(ID "value")
(NEWLINE)
(KEYWORD if)
(ID "self")
(PUNCT ".")
(ID "_parsing")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "_reset_lex_handler_prop")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD elif)
(ID "name")
(PUNCT "==")
(ID "property_interning_dict")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "_interning")
(PUNCT "=")
(ID "value")
(NEWLINE)
(DEDENT)
(KEYWORD elif)
(ID "name")
(PUNCT "==")
(ID "property_xml_string")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD raise)
(ID "SAXNotSupportedException")
(PUNCT "(")
(LIT "Property '%s' cannot be set")
(PUNCT "%")
(ID "name")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD raise)
(ID "SAXNotRecognizedException")
(PUNCT "(")
(LIT "Property '%s' not recognized")
(PUNCT "%")
(ID "name")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD def)
(ID "feed")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "data")
(PUNCT ",")
(ID "isFinal")
(PUNCT "=")
(LIT 0)
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(KEYWORD not)
(ID "self")
(PUNCT ".")
(ID "_parsing")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "reset")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_parsing")
(PUNCT "=")
(LIT 1)
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_cont_handler")
(PUNCT ".")
(ID "startDocument")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD try)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "_parser")
(PUNCT ".")
(ID "Parse")
(PUNCT "(")
(ID "data")
(PUNCT ",")
(ID "isFinal")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD except)
(ID "expat")
(PUNCT ".")
(ID "error")
(KEYWORD as)
(ID "e")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "exc")
(PUNCT "=")
(ID "SAXParseException")
(PUNCT "(")
(ID "expat")
(PUNCT ".")
(ID "ErrorString")
(PUNCT "(")
(ID "e")
(PUNCT ".")
(ID "code")
(PUNCT ")")
(PUNCT ",")
(ID "e")
(PUNCT ",")
(ID "self")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_err_handler")
(PUNCT ".")
(ID "fatalError")
(PUNCT "(")
(ID "exc")
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
(KEYWORD if)
(ID "self")
(PUNCT ".")
(ID "_entity_stack")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(NEWLINE)
(DEDENT)
(ID "self")
(PUNCT ".")
(ID "feed")
(PUNCT "(")
(LIT "")
(PUNCT ",")
(ID "isFinal")
(PUNCT "=")
(LIT 1)
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_cont_handler")
(PUNCT ".")
(ID "endDocument")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_parsing")
(PUNCT "=")
(LIT 0)
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_parser")
(PUNCT "=")
(KEYWORD None)
(NEWLINE)
(ID "bs")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "_source")
(PUNCT ".")
(ID "getByteStream")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(ID "bs")
(KEYWORD is)
(KEYWORD not)
(KEYWORD None)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "bs")
(PUNCT ".")
(ID "close")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD def)
(ID "_reset_cont_handler")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "_parser")
(PUNCT ".")
(ID "ProcessingInstructionHandler")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "_cont_handler")
(PUNCT ".")
(ID "processingInstruction")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_parser")
(PUNCT ".")
(ID "CharacterDataHandler")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "_cont_handler")
(PUNCT ".")
(ID "characters")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "_reset_lex_handler_prop")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "lex")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "_lex_handler_prop")
(NEWLINE)
(ID "parser")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "_parser")
(NEWLINE)
(KEYWORD if)
(ID "lex")
(KEYWORD is)
(KEYWORD None)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "parser")
(PUNCT ".")
(ID "CommentHandler")
(PUNCT "=")
(KEYWORD None)
(NEWLINE)
(ID "parser")
(PUNCT ".")
(ID "StartCdataSectionHandler")
(PUNCT "=")
(KEYWORD None)
(NEWLINE)
(ID "parser")
(PUNCT ".")
(ID "EndCdataSectionHandler")
(PUNCT "=")
(KEYWORD None)
(NEWLINE)
(ID "parser")
(PUNCT ".")
(ID "StartDoctypeDeclHandler")
(PUNCT "=")
(KEYWORD None)
(NEWLINE)
(ID "parser")
(PUNCT ".")
(ID "EndDoctypeDeclHandler")
(PUNCT "=")
(KEYWORD None)
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "parser")
(PUNCT ".")
(ID "CommentHandler")
(PUNCT "=")
(ID "lex")
(PUNCT ".")
(ID "comment")
(NEWLINE)
(ID "parser")
(PUNCT ".")
(ID "StartCdataSectionHandler")
(PUNCT "=")
(ID "lex")
(PUNCT ".")
(ID "startCDATA")
(NEWLINE)
(ID "parser")
(PUNCT ".")
(ID "EndCdataSectionHandler")
(PUNCT "=")
(ID "lex")
(PUNCT ".")
(ID "endCDATA")
(NEWLINE)
(ID "parser")
(PUNCT ".")
(ID "StartDoctypeDeclHandler")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "start_doctype_decl")
(NEWLINE)
(ID "parser")
(PUNCT ".")
(ID "EndDoctypeDeclHandler")
(PUNCT "=")
(ID "lex")
(PUNCT ".")
(ID "endDTD")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD def)
(ID "reset")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "self")
(PUNCT ".")
(ID "_namespaces")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "_parser")
(PUNCT "=")
(ID "expat")
(PUNCT ".")
(ID "ParserCreate")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "_source")
(PUNCT ".")
(ID "getEncoding")
(PUNCT "(")
(PUNCT ")")
(PUNCT ",")
(LIT " ")
(PUNCT ",")
(ID "intern")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "_interning")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_parser")
(PUNCT ".")
(ID "namespace_prefixes")
(PUNCT "=")
(LIT 1)
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_parser")
(PUNCT ".")
(ID "StartElementHandler")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "start_element_ns")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_parser")
(PUNCT ".")
(ID "EndElementHandler")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "end_element_ns")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "_parser")
(PUNCT "=")
(ID "expat")
(PUNCT ".")
(ID "ParserCreate")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "_source")
(PUNCT ".")
(ID "getEncoding")
(PUNCT "(")
(PUNCT ")")
(PUNCT ",")
(ID "intern")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "_interning")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_parser")
(PUNCT ".")
(ID "StartElementHandler")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "start_element")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_parser")
(PUNCT ".")
(ID "EndElementHandler")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "end_element")
(NEWLINE)
(DEDENT)
(ID "self")
(PUNCT ".")
(ID "_reset_cont_handler")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_parser")
(PUNCT ".")
(ID "UnparsedEntityDeclHandler")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "unparsed_entity_decl")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_parser")
(PUNCT ".")
(ID "NotationDeclHandler")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "notation_decl")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_parser")
(PUNCT ".")
(ID "StartNamespaceDeclHandler")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "start_namespace_decl")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_parser")
(PUNCT ".")
(ID "EndNamespaceDeclHandler")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "end_namespace_decl")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_decl_handler_prop")
(PUNCT "=")
(KEYWORD None)
(NEWLINE)
(KEYWORD if)
(ID "self")
(PUNCT ".")
(ID "_lex_handler_prop")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "_reset_lex_handler_prop")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "self")
(PUNCT ".")
(ID "_parser")
(PUNCT ".")
(ID "ExternalEntityRefHandler")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "external_entity_ref")
(NEWLINE)
(KEYWORD try)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "_parser")
(PUNCT ".")
(ID "SkippedEntityHandler")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "skipped_entity_handler")
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
(ID "self")
(PUNCT ".")
(ID "_parser")
(PUNCT ".")
(ID "SetParamEntityParsing")
(PUNCT "(")
(ID "expat")
(PUNCT ".")
(ID "XML_PARAM_ENTITY_PARSING_UNLESS_STANDALONE")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_parsing")
(PUNCT "=")
(LIT 0)
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_entity_stack")
(PUNCT "=")
(PUNCT "[")
(PUNCT "]")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "getColumnNumber")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "self")
(PUNCT ".")
(ID "_parser")
(KEYWORD is)
(KEYWORD None)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(KEYWORD None)
(NEWLINE)
(DEDENT)
(KEYWORD return)
(ID "self")
(PUNCT ".")
(ID "_parser")
(PUNCT ".")
(ID "ErrorColumnNumber")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "getLineNumber")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "self")
(PUNCT ".")
(ID "_parser")
(KEYWORD is)
(KEYWORD None)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(LIT 1)
(NEWLINE)
(DEDENT)
(KEYWORD return)
(ID "self")
(PUNCT ".")
(ID "_parser")
(PUNCT ".")
(ID "ErrorLineNumber")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "getPublicId")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(ID "self")
(PUNCT ".")
(ID "_source")
(PUNCT ".")
(ID "getPublicId")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "getSystemId")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(ID "self")
(PUNCT ".")
(ID "_source")
(PUNCT ".")
(ID "getSystemId")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "start_element")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "name")
(PUNCT ",")
(ID "attrs")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "_cont_handler")
(PUNCT ".")
(ID "startElement")
(PUNCT "(")
(ID "name")
(PUNCT ",")
(ID "AttributesImpl")
(PUNCT "(")
(ID "attrs")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "end_element")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "name")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "_cont_handler")
(PUNCT ".")
(ID "endElement")
(PUNCT "(")
(ID "name")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "start_element_ns")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "name")
(PUNCT ",")
(ID "attrs")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "pair")
(PUNCT "=")
(ID "name")
(PUNCT ".")
(ID "split")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(ID "len")
(PUNCT "(")
(ID "pair")
(PUNCT ")")
(PUNCT "==")
(LIT 1)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "pair")
(PUNCT "=")
(PUNCT "(")
(KEYWORD None)
(PUNCT ",")
(ID "name")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD elif)
(ID "len")
(PUNCT "(")
(ID "pair")
(PUNCT ")")
(PUNCT "==")
(LIT 3)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "pair")
(PUNCT "=")
(ID "pair")
(PUNCT "[")
(LIT 0)
(PUNCT "]")
(PUNCT ",")
(ID "pair")
(PUNCT "[")
(LIT 1)
(PUNCT "]")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "pair")
(PUNCT "=")
(ID "tuple")
(PUNCT "(")
(ID "pair")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "newattrs")
(PUNCT "=")
(PUNCT "{")
(PUNCT "}")
(NEWLINE)
(ID "qnames")
(PUNCT "=")
(PUNCT "{")
(PUNCT "}")
(NEWLINE)
(KEYWORD for)
(PUNCT "(")
(ID "aname")
(PUNCT ",")
(ID "value")
(PUNCT ")")
(KEYWORD in)
(ID "attrs")
(PUNCT ".")
(ID "items")
(PUNCT "(")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "parts")
(PUNCT "=")
(ID "aname")
(PUNCT ".")
(ID "split")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "length")
(PUNCT "=")
(ID "len")
(PUNCT "(")
(ID "parts")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(ID "length")
(PUNCT "==")
(LIT 1)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "qname")
(PUNCT "=")
(ID "aname")
(NEWLINE)
(ID "apair")
(PUNCT "=")
(PUNCT "(")
(KEYWORD None)
(PUNCT ",")
(ID "aname")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD elif)
(ID "length")
(PUNCT "==")
(LIT 3)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "qname")
(PUNCT "=")
(LIT "%s:%s")
(PUNCT "%")
(PUNCT "(")
(ID "parts")
(PUNCT "[")
(LIT 2)
(PUNCT "]")
(PUNCT ",")
(ID "parts")
(PUNCT "[")
(LIT 1)
(PUNCT "]")
(PUNCT ")")
(NEWLINE)
(ID "apair")
(PUNCT "=")
(ID "parts")
(PUNCT "[")
(LIT 0)
(PUNCT "]")
(PUNCT ",")
(ID "parts")
(PUNCT "[")
(LIT 1)
(PUNCT "]")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "qname")
(PUNCT "=")
(ID "parts")
(PUNCT "[")
(LIT 1)
(PUNCT "]")
(NEWLINE)
(ID "apair")
(PUNCT "=")
(ID "tuple")
(PUNCT "(")
(ID "parts")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "newattrs")
(PUNCT "[")
(ID "apair")
(PUNCT "]")
(PUNCT "=")
(ID "value")
(NEWLINE)
(ID "qnames")
(PUNCT "[")
(ID "apair")
(PUNCT "]")
(PUNCT "=")
(ID "qname")
(NEWLINE)
(DEDENT)
(ID "self")
(PUNCT ".")
(ID "_cont_handler")
(PUNCT ".")
(ID "startElementNS")
(PUNCT "(")
(ID "pair")
(PUNCT ",")
(KEYWORD None)
(PUNCT ",")
(ID "AttributesNSImpl")
(PUNCT "(")
(ID "newattrs")
(PUNCT ",")
(ID "qnames")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "end_element_ns")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "name")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "pair")
(PUNCT "=")
(ID "name")
(PUNCT ".")
(ID "split")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(ID "len")
(PUNCT "(")
(ID "pair")
(PUNCT ")")
(PUNCT "==")
(LIT 1)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "pair")
(PUNCT "=")
(PUNCT "(")
(KEYWORD None)
(PUNCT ",")
(ID "name")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD elif)
(ID "len")
(PUNCT "(")
(ID "pair")
(PUNCT ")")
(PUNCT "==")
(LIT 3)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "pair")
(PUNCT "=")
(ID "pair")
(PUNCT "[")
(LIT 0)
(PUNCT "]")
(PUNCT ",")
(ID "pair")
(PUNCT "[")
(LIT 1)
(PUNCT "]")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "pair")
(PUNCT "=")
(ID "tuple")
(PUNCT "(")
(ID "pair")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "self")
(PUNCT ".")
(ID "_cont_handler")
(PUNCT ".")
(ID "endElementNS")
(PUNCT "(")
(ID "pair")
(PUNCT ",")
(KEYWORD None)
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "processing_instruction")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "target")
(PUNCT ",")
(ID "data")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "_cont_handler")
(PUNCT ".")
(ID "processingInstruction")
(PUNCT "(")
(ID "target")
(PUNCT ",")
(ID "data")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "character_data")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "data")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "_cont_handler")
(PUNCT ".")
(ID "characters")
(PUNCT "(")
(ID "data")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "start_namespace_decl")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "prefix")
(PUNCT ",")
(ID "uri")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "_cont_handler")
(PUNCT ".")
(ID "startPrefixMapping")
(PUNCT "(")
(ID "prefix")
(PUNCT ",")
(ID "uri")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "end_namespace_decl")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "prefix")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "_cont_handler")
(PUNCT ".")
(ID "endPrefixMapping")
(PUNCT "(")
(ID "prefix")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "start_doctype_decl")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "name")
(PUNCT ",")
(ID "sysid")
(PUNCT ",")
(ID "pubid")
(PUNCT ",")
(ID "has_internal_subset")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "_lex_handler_prop")
(PUNCT ".")
(ID "startDTD")
(PUNCT "(")
(ID "name")
(PUNCT ",")
(ID "pubid")
(PUNCT ",")
(ID "sysid")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "unparsed_entity_decl")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "name")
(PUNCT ",")
(ID "base")
(PUNCT ",")
(ID "sysid")
(PUNCT ",")
(ID "pubid")
(PUNCT ",")
(ID "notation_name")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "_dtd_handler")
(PUNCT ".")
(ID "unparsedEntityDecl")
(PUNCT "(")
(ID "name")
(PUNCT ",")
(ID "pubid")
(PUNCT ",")
(ID "sysid")
(PUNCT ",")
(ID "notation_name")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "notation_decl")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "name")
(PUNCT ",")
(ID "base")
(PUNCT ",")
(ID "sysid")
(PUNCT ",")
(ID "pubid")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "_dtd_handler")
(PUNCT ".")
(ID "notationDecl")
(PUNCT "(")
(ID "name")
(PUNCT ",")
(ID "pubid")
(PUNCT ",")
(ID "sysid")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "external_entity_ref")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "context")
(PUNCT ",")
(ID "base")
(PUNCT ",")
(ID "sysid")
(PUNCT ",")
(ID "pubid")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(KEYWORD not)
(ID "self")
(PUNCT ".")
(ID "_external_ges")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(LIT 1)
(NEWLINE)
(DEDENT)
(ID "source")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "_ent_handler")
(PUNCT ".")
(ID "resolveEntity")
(PUNCT "(")
(ID "pubid")
(PUNCT ",")
(ID "sysid")
(PUNCT ")")
(NEWLINE)
(ID "source")
(PUNCT "=")
(ID "saxutils")
(PUNCT ".")
(ID "prepare_input_source")
(PUNCT "(")
(ID "source")
(PUNCT ",")
(ID "self")
(PUNCT ".")
(ID "_source")
(PUNCT ".")
(ID "getSystemId")
(PUNCT "(")
(PUNCT ")")
(KEYWORD or)
(LIT "")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_entity_stack")
(PUNCT ".")
(ID "append")
(PUNCT "(")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "_parser")
(PUNCT ",")
(ID "self")
(PUNCT ".")
(ID "_source")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_parser")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "_parser")
(PUNCT ".")
(ID "ExternalEntityParserCreate")
(PUNCT "(")
(ID "context")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_source")
(PUNCT "=")
(ID "source")
(NEWLINE)
(KEYWORD try)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "xmlreader")
(PUNCT ".")
(ID "IncrementalParser")
(PUNCT ".")
(ID "parse")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "source")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD except)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(LIT 0)
(NEWLINE)
(DEDENT)
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "_parser")
(PUNCT ",")
(ID "self")
(PUNCT ".")
(ID "_source")
(PUNCT ")")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "_entity_stack")
(PUNCT "[")
(PUNCT "-")
(LIT 1)
(PUNCT "]")
(NEWLINE)
(KEYWORD del)
(ID "self")
(PUNCT ".")
(ID "_entity_stack")
(PUNCT "[")
(PUNCT "-")
(LIT 1)
(PUNCT "]")
(NEWLINE)
(KEYWORD return)
(LIT 1)
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "skipped_entity_handler")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "name")
(PUNCT ",")
(ID "is_pe")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "is_pe")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "name")
(PUNCT "=")
(LIT "%")
(PUNCT "+")
(ID "name")
(NEWLINE)
(DEDENT)
(ID "self")
(PUNCT ".")
(ID "_cont_handler")
(PUNCT ".")
(ID "skippedEntity")
(PUNCT "(")
(ID "name")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD def)
(ID "create_parser")
(PUNCT "(")
(PUNCT "*")
(ID "args")
(PUNCT ",")
(PUNCT "**")
(ID "kwargs")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(ID "ExpatParser")
(PUNCT "(")
(PUNCT "*")
(ID "args")
(PUNCT ",")
(PUNCT "**")
(ID "kwargs")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD if)
(ID "__name__")
(PUNCT "==")
(LIT "__main__")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD import)
(ID "xml")
(PUNCT ".")
(ID "sax")
(PUNCT ".")
(ID "saxutils")
(NEWLINE)
(ID "p")
(PUNCT "=")
(ID "create_parser")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "p")
(PUNCT ".")
(ID "setContentHandler")
(PUNCT "(")
(ID "xml")
(PUNCT ".")
(ID "sax")
(PUNCT ".")
(ID "saxutils")
(PUNCT ".")
(ID "XMLGenerator")
(PUNCT "(")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(ID "p")
(PUNCT ".")
(ID "setErrorHandler")
(PUNCT "(")
(ID "xml")
(PUNCT ".")
(ID "sax")
(PUNCT ".")
(ID "ErrorHandler")
(PUNCT "(")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(ID "p")
(PUNCT ".")
(ID "parse")
(PUNCT "(")
(LIT "http://www.ibiblio.org/xml/examples/shakespeare/hamlet.xml")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ENDMARKER)
