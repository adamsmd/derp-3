(LIT "Module for parsing and testing package version predicate strings.\n")
(NEWLINE)
(KEYWORD import)
(ID "re")
(NEWLINE)
(KEYWORD import)
(ID "distutils")
(PUNCT ".")
(ID "version")
(NEWLINE)
(KEYWORD import)
(ID "operator")
(NEWLINE)
(ID "re_validPackage")
(PUNCT "=")
(ID "re")
(PUNCT ".")
(ID "compile")
(PUNCT "(")
(LIT "(?i)^\\s*([a-z_]\\w*(?:\\.[a-z_]\\w*)*)(.*)")
(PUNCT ",")
(ID "re")
(PUNCT ".")
(ID "ASCII")
(PUNCT ")")
(NEWLINE)
(ID "re_paren")
(PUNCT "=")
(ID "re")
(PUNCT ".")
(ID "compile")
(PUNCT "(")
(LIT "^\\s*\\((.*)\\)\\s*$")
(PUNCT ")")
(NEWLINE)
(ID "re_splitComparison")
(PUNCT "=")
(ID "re")
(PUNCT ".")
(ID "compile")
(PUNCT "(")
(LIT "^\\s*(<=|>=|<|>|!=|==)\\s*([^\\s,]+)\\s*$")
(PUNCT ")")
(NEWLINE)
(KEYWORD def)
(ID "splitUp")
(PUNCT "(")
(ID "pred")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Parse a single version comparison.\n\n    Return (comparison string, StrictVersion)\n    ")
(NEWLINE)
(ID "res")
(PUNCT "=")
(ID "re_splitComparison")
(PUNCT ".")
(ID "match")
(PUNCT "(")
(ID "pred")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(KEYWORD not)
(ID "res")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD raise)
(ID "ValueError")
(PUNCT "(")
(LIT "bad package restriction syntax: %r")
(PUNCT "%")
(ID "pred")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "comp")
(PUNCT ",")
(ID "verStr")
(PUNCT "=")
(ID "res")
(PUNCT ".")
(ID "groups")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(KEYWORD return)
(PUNCT "(")
(ID "comp")
(PUNCT ",")
(ID "distutils")
(PUNCT ".")
(ID "version")
(PUNCT ".")
(ID "StrictVersion")
(PUNCT "(")
(ID "verStr")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "compmap")
(PUNCT "=")
(PUNCT "{")
(LIT "<")
(PUNCT ":")
(ID "operator")
(PUNCT ".")
(ID "lt")
(PUNCT ",")
(LIT "<=")
(PUNCT ":")
(ID "operator")
(PUNCT ".")
(ID "le")
(PUNCT ",")
(LIT "==")
(PUNCT ":")
(ID "operator")
(PUNCT ".")
(ID "eq")
(PUNCT ",")
(LIT ">")
(PUNCT ":")
(ID "operator")
(PUNCT ".")
(ID "gt")
(PUNCT ",")
(LIT ">=")
(PUNCT ":")
(ID "operator")
(PUNCT ".")
(ID "ge")
(PUNCT ",")
(LIT "!=")
(PUNCT ":")
(ID "operator")
(PUNCT ".")
(ID "ne")
(PUNCT "}")
(NEWLINE)
(KEYWORD class)
(ID "VersionPredicate")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Parse and test package version predicates.\n\n    >>> v = VersionPredicate('pyepat.abc (>1.0, <3333.3a1, !=1555.1b3)')\n\n    The `name` attribute provides the full dotted name that is given::\n\n    >>> v.name\n    'pyepat.abc'\n\n    The str() of a `VersionPredicate` provides a normalized\n    human-readable version of the expression::\n\n    >>> print(v)\n    pyepat.abc (> 1.0, < 3333.3a1, != 1555.1b3)\n\n    The `satisfied_by()` method can be used to determine with a given\n    version number is included in the set described by the version\n    restrictions::\n\n    >>> v.satisfied_by('1.1')\n    True\n    >>> v.satisfied_by('1.4')\n    True\n    >>> v.satisfied_by('1.0')\n    False\n    >>> v.satisfied_by('4444.4')\n    False\n    >>> v.satisfied_by('1555.1b3')\n    False\n\n    `VersionPredicate` is flexible in accepting extra whitespace::\n\n    >>> v = VersionPredicate(' pat( ==  0.1  )  ')\n    >>> v.name\n    'pat'\n    >>> v.satisfied_by('0.1')\n    True\n    >>> v.satisfied_by('0.2')\n    False\n\n    If any version numbers passed in do not conform to the\n    restrictions of `StrictVersion`, a `ValueError` is raised::\n\n    >>> v = VersionPredicate('p1.p2.p3.p4(>=1.0, <=1.3a1, !=1.2zb3)')\n    Traceback (most recent call last):\n      ...\n    ValueError: invalid version number '1.2zb3'\n\n    It the module or package name given does not conform to what's\n    allowed as a legal module or package name, `ValueError` is\n    raised::\n\n    >>> v = VersionPredicate('foo-bar')\n    Traceback (most recent call last):\n      ...\n    ValueError: expected parenthesized list: '-bar'\n\n    >>> v = VersionPredicate('foo bar (12.21)')\n    Traceback (most recent call last):\n      ...\n    ValueError: expected parenthesized list: 'bar (12.21)'\n\n    ")
(NEWLINE)
(KEYWORD def)
(ID "__init__")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "versionPredicateStr")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Parse a version predicate string.\n        ")
(NEWLINE)
(ID "versionPredicateStr")
(PUNCT "=")
(ID "versionPredicateStr")
(PUNCT ".")
(ID "strip")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(KEYWORD not)
(ID "versionPredicateStr")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD raise)
(ID "ValueError")
(PUNCT "(")
(LIT "empty package restriction")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "match")
(PUNCT "=")
(ID "re_validPackage")
(PUNCT ".")
(ID "match")
(PUNCT "(")
(ID "versionPredicateStr")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(KEYWORD not)
(ID "match")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD raise)
(ID "ValueError")
(PUNCT "(")
(LIT "bad package name in %r")
(PUNCT "%")
(ID "versionPredicateStr")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "self")
(PUNCT ".")
(ID "name")
(PUNCT ",")
(ID "paren")
(PUNCT "=")
(ID "match")
(PUNCT ".")
(ID "groups")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "paren")
(PUNCT "=")
(ID "paren")
(PUNCT ".")
(ID "strip")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(ID "paren")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "match")
(PUNCT "=")
(ID "re_paren")
(PUNCT ".")
(ID "match")
(PUNCT "(")
(ID "paren")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(KEYWORD not)
(ID "match")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD raise)
(ID "ValueError")
(PUNCT "(")
(LIT "expected parenthesized list: %r")
(PUNCT "%")
(ID "paren")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "str")
(PUNCT "=")
(ID "match")
(PUNCT ".")
(ID "groups")
(PUNCT "(")
(PUNCT ")")
(PUNCT "[")
(LIT 0)
(PUNCT "]")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "pred")
(PUNCT "=")
(PUNCT "[")
(ID "splitUp")
(PUNCT "(")
(ID "aPred")
(PUNCT ")")
(KEYWORD for)
(ID "aPred")
(KEYWORD in)
(ID "str")
(PUNCT ".")
(ID "split")
(PUNCT "(")
(LIT ",")
(PUNCT ")")
(PUNCT "]")
(NEWLINE)
(KEYWORD if)
(KEYWORD not)
(ID "self")
(PUNCT ".")
(ID "pred")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD raise)
(ID "ValueError")
(PUNCT "(")
(LIT "empty parenthesized list in %r")
(PUNCT "%")
(ID "versionPredicateStr")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "pred")
(PUNCT "=")
(PUNCT "[")
(PUNCT "]")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD def)
(ID "__str__")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "self")
(PUNCT ".")
(ID "pred")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "seq")
(PUNCT "=")
(PUNCT "[")
(ID "cond")
(PUNCT "+")
(LIT " ")
(PUNCT "+")
(ID "str")
(PUNCT "(")
(ID "ver")
(PUNCT ")")
(KEYWORD for)
(ID "cond")
(PUNCT ",")
(ID "ver")
(KEYWORD in)
(ID "self")
(PUNCT ".")
(ID "pred")
(PUNCT "]")
(NEWLINE)
(KEYWORD return)
(ID "self")
(PUNCT ".")
(ID "name")
(PUNCT "+")
(LIT " (")
(PUNCT "+")
(LIT ", ")
(PUNCT ".")
(ID "join")
(PUNCT "(")
(ID "seq")
(PUNCT ")")
(PUNCT "+")
(LIT ")")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(ID "self")
(PUNCT ".")
(ID "name")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD def)
(ID "satisfied_by")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "version")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "True if version is compatible with all the predicates in self.\n        The parameter version must be acceptable to the StrictVersion\n        constructor.  It may be either a string or StrictVersion.\n        ")
(NEWLINE)
(KEYWORD for)
(ID "cond")
(PUNCT ",")
(ID "ver")
(KEYWORD in)
(ID "self")
(PUNCT ".")
(ID "pred")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(KEYWORD not)
(ID "compmap")
(PUNCT "[")
(ID "cond")
(PUNCT "]")
(PUNCT "(")
(ID "version")
(PUNCT ",")
(ID "ver")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(KEYWORD False)
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD return)
(KEYWORD True)
(NEWLINE)
(DEDENT)
(DEDENT)
(ID "_provision_rx")
(PUNCT "=")
(KEYWORD None)
(NEWLINE)
(KEYWORD def)
(ID "split_provision")
(PUNCT "(")
(ID "value")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Return the name and optional version number of a provision.\n\n    The version number, if given, will be returned as a `StrictVersion`\n    instance, otherwise it will be `None`.\n\n    >>> split_provision('mypkg')\n    ('mypkg', None)\n    >>> split_provision(' mypkg( 1.2 ) ')\n    ('mypkg', StrictVersion ('1.2'))\n    ")
(NEWLINE)
(KEYWORD global)
(ID "_provision_rx")
(NEWLINE)
(KEYWORD if)
(ID "_provision_rx")
(KEYWORD is)
(KEYWORD None)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "_provision_rx")
(PUNCT "=")
(ID "re")
(PUNCT ".")
(ID "compile")
(PUNCT "(")
(LIT "([a-zA-Z_]\\w*(?:\\.[a-zA-Z_]\\w*)*)(?:\\s*\\(\\s*([^)\\s]+)\\s*\\))?$")
(PUNCT ",")
(ID "re")
(PUNCT ".")
(ID "ASCII")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "value")
(PUNCT "=")
(ID "value")
(PUNCT ".")
(ID "strip")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "m")
(PUNCT "=")
(ID "_provision_rx")
(PUNCT ".")
(ID "match")
(PUNCT "(")
(ID "value")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(KEYWORD not)
(ID "m")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD raise)
(ID "ValueError")
(PUNCT "(")
(LIT "illegal provides specification: %r")
(PUNCT "%")
(ID "value")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "ver")
(PUNCT "=")
(ID "m")
(PUNCT ".")
(ID "group")
(PUNCT "(")
(LIT 2)
(PUNCT ")")
(KEYWORD or)
(KEYWORD None)
(NEWLINE)
(KEYWORD if)
(ID "ver")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "ver")
(PUNCT "=")
(ID "distutils")
(PUNCT ".")
(ID "version")
(PUNCT ".")
(ID "StrictVersion")
(PUNCT "(")
(ID "ver")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD return)
(ID "m")
(PUNCT ".")
(ID "group")
(PUNCT "(")
(LIT 1)
(PUNCT ")")
(PUNCT ",")
(ID "ver")
(NEWLINE)
(DEDENT)
(ENDMARKER)
