(LIT "\nThis module contains the core classes of version 2.0 of SAX for Python.\nThis file provides only default classes with absolutely minimum\nfunctionality, from which drivers and applications can be subclassed.\n\nMany of these classes are empty and are included only as documentation\nof the interfaces.\n\n$Id$\n")
(NEWLINE)
(ID "version")
(PUNCT "=")
(LIT "2.0beta")
(NEWLINE)
(KEYWORD class)
(ID "ErrorHandler")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Basic interface for SAX error handlers.\n\n    If you create an object that implements this interface, then\n    register the object with your XMLReader, the parser will call the\n    methods in your object to report all warnings and errors. There\n    are three levels of errors available: warnings, (possibly)\n    recoverable errors, and unrecoverable errors. All methods take a\n    SAXParseException as the only parameter.")
(NEWLINE)
(KEYWORD def)
(ID "error")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "exception")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Handle a recoverable error.")
(NEWLINE)
(KEYWORD raise)
(ID "exception")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "fatalError")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "exception")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Handle a non-recoverable error.")
(NEWLINE)
(KEYWORD raise)
(ID "exception")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "warning")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "exception")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Handle a warning.")
(NEWLINE)
(ID "print")
(PUNCT "(")
(ID "exception")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD class)
(ID "ContentHandler")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Interface for receiving logical document content events.\n\n    This is the main callback interface in SAX, and the one most\n    important to applications. The order of events in this interface\n    mirrors the order of the information in the document.")
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
(ID "_locator")
(PUNCT "=")
(KEYWORD None)
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "setDocumentLocator")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "locator")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Called by the parser to give the application a locator for\n        locating the origin of document events.\n\n        SAX parsers are strongly encouraged (though not absolutely\n        required) to supply a locator: if it does so, it must supply\n        the locator to the application by invoking this method before\n        invoking any of the other methods in the DocumentHandler\n        interface.\n\n        The locator allows the application to determine the end\n        position of any document-related event, even if the parser is\n        not reporting an error. Typically, the application will use\n        this information for reporting its own errors (such as\n        character content that does not match an application's\n        business rules). The information returned by the locator is\n        probably not sufficient for use with a search engine.\n\n        Note that the locator will return correct information only\n        during the invocation of the events in this interface. The\n        application should not attempt to use it at any other time.")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_locator")
(PUNCT "=")
(ID "locator")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "startDocument")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Receive notification of the beginning of a document.\n\n        The SAX parser will invoke this method only once, before any\n        other methods in this interface or in DTDHandler (except for\n        setDocumentLocator).")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "endDocument")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Receive notification of the end of a document.\n\n        The SAX parser will invoke this method only once, and it will\n        be the last method invoked during the parse. The parser shall\n        not invoke this method until it has either abandoned parsing\n        (because of an unrecoverable error) or reached the end of\n        input.")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "startPrefixMapping")
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
(LIT "Begin the scope of a prefix-URI Namespace mapping.\n\n        The information from this event is not necessary for normal\n        Namespace processing: the SAX XML reader will automatically\n        replace prefixes for element and attribute names when the\n        http://xml.org/sax/features/namespaces feature is true (the\n        default).\n\n        There are cases, however, when applications need to use\n        prefixes in character data or in attribute values, where they\n        cannot safely be expanded automatically; the\n        start/endPrefixMapping event supplies the information to the\n        application to expand prefixes in those contexts itself, if\n        necessary.\n\n        Note that start/endPrefixMapping events are not guaranteed to\n        be properly nested relative to each-other: all\n        startPrefixMapping events will occur before the corresponding\n        startElement event, and all endPrefixMapping events will occur\n        after the corresponding endElement event, but their order is\n        not guaranteed.")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "endPrefixMapping")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "prefix")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "End the scope of a prefix-URI mapping.\n\n        See startPrefixMapping for details. This event will always\n        occur after the corresponding endElement event, but the order\n        of endPrefixMapping events is not otherwise guaranteed.")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "startElement")
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
(LIT "Signals the start of an element in non-namespace mode.\n\n        The name parameter contains the raw XML 1.0 name of the\n        element type as a string and the attrs parameter holds an\n        instance of the Attributes class containing the attributes of\n        the element.")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "endElement")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "name")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Signals the end of an element in non-namespace mode.\n\n        The name parameter contains the name of the element type, just\n        as with the startElement event.")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "startElementNS")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "name")
(PUNCT ",")
(ID "qname")
(PUNCT ",")
(ID "attrs")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Signals the start of an element in namespace mode.\n\n        The name parameter contains the name of the element type as a\n        (uri, localname) tuple, the qname parameter the raw XML 1.0\n        name used in the source document, and the attrs parameter\n        holds an instance of the Attributes class containing the\n        attributes of the element.\n\n        The uri part of the name tuple is None for elements which have\n        no namespace.")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "endElementNS")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "name")
(PUNCT ",")
(ID "qname")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Signals the end of an element in namespace mode.\n\n        The name parameter contains the name of the element type, just\n        as with the startElementNS event.")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "characters")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "content")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Receive notification of character data.\n\n        The Parser will call this method to report each chunk of\n        character data. SAX parsers may return all contiguous\n        character data in a single chunk, or they may split it into\n        several chunks; however, all of the characters in any single\n        event must come from the same external entity so that the\n        Locator provides useful information.")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "ignorableWhitespace")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "whitespace")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Receive notification of ignorable whitespace in element content.\n\n        Validating Parsers must use this method to report each chunk\n        of ignorable whitespace (see the W3C XML 1.0 recommendation,\n        section 2.10): non-validating parsers may also use this method\n        if they are capable of parsing and using content models.\n\n        SAX parsers may return all contiguous whitespace in a single\n        chunk, or they may split it into several chunks; however, all\n        of the characters in any single event must come from the same\n        external entity, so that the Locator provides useful\n        information.")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "processingInstruction")
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
(LIT "Receive notification of a processing instruction.\n\n        The Parser will invoke this method once for each processing\n        instruction found: note that processing instructions may occur\n        before or after the main document element.\n\n        A SAX parser should never report an XML declaration (XML 1.0,\n        section 2.8) or a text declaration (XML 1.0, section 4.3.1)\n        using this method.")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "skippedEntity")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "name")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Receive notification of a skipped entity.\n\n        The Parser will invoke this method once for each entity\n        skipped. Non-validating processors may skip entities if they\n        have not seen the declarations (because, for example, the\n        entity was declared in an external DTD subset). All processors\n        may skip external entities, depending on the values of the\n        http://xml.org/sax/features/external-general-entities and the\n        http://xml.org/sax/features/external-parameter-entities\n        properties.")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD class)
(ID "DTDHandler")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Handle DTD events.\n\n    This interface specifies only those DTD events required for basic\n    parsing (unparsed entities and attributes).")
(NEWLINE)
(KEYWORD def)
(ID "notationDecl")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "name")
(PUNCT ",")
(ID "publicId")
(PUNCT ",")
(ID "systemId")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Handle a notation declaration event.")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "unparsedEntityDecl")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "name")
(PUNCT ",")
(ID "publicId")
(PUNCT ",")
(ID "systemId")
(PUNCT ",")
(ID "ndata")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Handle an unparsed entity declaration event.")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD class)
(ID "EntityResolver")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Basic interface for resolving entities. If you create an object\n    implementing this interface, then register the object with your\n    Parser, the parser will call the method in your object to\n    resolve all external entities. Note that DefaultHandler implements\n    this interface with the default behaviour.")
(NEWLINE)
(KEYWORD def)
(ID "resolveEntity")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "publicId")
(PUNCT ",")
(ID "systemId")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Resolve the system identifier of an entity and return either\n        the system identifier to read from as a string, or an InputSource\n        to read from.")
(NEWLINE)
(KEYWORD return)
(ID "systemId")
(NEWLINE)
(DEDENT)
(DEDENT)
(ID "feature_namespaces")
(PUNCT "=")
(LIT "http://xml.org/sax/features/namespaces")
(NEWLINE)
(ID "feature_namespace_prefixes")
(PUNCT "=")
(LIT "http://xml.org/sax/features/namespace-prefixes")
(NEWLINE)
(ID "feature_string_interning")
(PUNCT "=")
(LIT "http://xml.org/sax/features/string-interning")
(NEWLINE)
(ID "feature_validation")
(PUNCT "=")
(LIT "http://xml.org/sax/features/validation")
(NEWLINE)
(ID "feature_external_ges")
(PUNCT "=")
(LIT "http://xml.org/sax/features/external-general-entities")
(NEWLINE)
(ID "feature_external_pes")
(PUNCT "=")
(LIT "http://xml.org/sax/features/external-parameter-entities")
(NEWLINE)
(ID "all_features")
(PUNCT "=")
(PUNCT "[")
(ID "feature_namespaces")
(PUNCT ",")
(ID "feature_namespace_prefixes")
(PUNCT ",")
(ID "feature_string_interning")
(PUNCT ",")
(ID "feature_validation")
(PUNCT ",")
(ID "feature_external_ges")
(PUNCT ",")
(ID "feature_external_pes")
(PUNCT "]")
(NEWLINE)
(ID "property_lexical_handler")
(PUNCT "=")
(LIT "http://xml.org/sax/properties/lexical-handler")
(NEWLINE)
(ID "property_declaration_handler")
(PUNCT "=")
(LIT "http://xml.org/sax/properties/declaration-handler")
(NEWLINE)
(ID "property_dom_node")
(PUNCT "=")
(LIT "http://xml.org/sax/properties/dom-node")
(NEWLINE)
(ID "property_xml_string")
(PUNCT "=")
(LIT "http://xml.org/sax/properties/xml-string")
(NEWLINE)
(ID "property_encoding")
(PUNCT "=")
(LIT "http://www.python.org/sax/properties/encoding")
(NEWLINE)
(ID "property_interning_dict")
(PUNCT "=")
(LIT "http://www.python.org/sax/properties/interning-dict")
(NEWLINE)
(ID "all_properties")
(PUNCT "=")
(PUNCT "[")
(ID "property_lexical_handler")
(PUNCT ",")
(ID "property_dom_node")
(PUNCT ",")
(ID "property_declaration_handler")
(PUNCT ",")
(ID "property_xml_string")
(PUNCT ",")
(ID "property_encoding")
(PUNCT ",")
(ID "property_interning_dict")
(PUNCT "]")
(NEWLINE)
(ENDMARKER)
