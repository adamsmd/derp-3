(LIT "distutils.pypirc\n\nProvides the PyPIRCCommand class, the base class for the command classes\nthat uses .pypirc in the distutils.command package.\n")
(NEWLINE)
(KEYWORD import)
(ID "os")
(NEWLINE)
(KEYWORD from)
(ID "configparser")
(KEYWORD import)
(ID "ConfigParser")
(NEWLINE)
(KEYWORD from)
(ID "distutils")
(PUNCT ".")
(ID "cmd")
(KEYWORD import)
(ID "Command")
(NEWLINE)
(ID "DEFAULT_PYPIRC")
(PUNCT "=")
(LIT "[distutils]\nindex-servers =\n    pypi\n\n[pypi]\nusername:%s\npassword:%s\n")
(NEWLINE)
(KEYWORD class)
(ID "PyPIRCCommand")
(PUNCT "(")
(ID "Command")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Base command that knows how to handle the .pypirc file\n    ")
(NEWLINE)
(ID "DEFAULT_REPOSITORY")
(PUNCT "=")
(LIT "https://pypi.python.org/pypi")
(NEWLINE)
(ID "DEFAULT_REALM")
(PUNCT "=")
(LIT "pypi")
(NEWLINE)
(ID "repository")
(PUNCT "=")
(KEYWORD None)
(NEWLINE)
(ID "realm")
(PUNCT "=")
(KEYWORD None)
(NEWLINE)
(ID "user_options")
(PUNCT "=")
(PUNCT "[")
(PUNCT "(")
(LIT "repository=")
(PUNCT ",")
(LIT "r")
(PUNCT ",")
(LIT "url of repository [default: %s]")
(PUNCT "%")
(ID "DEFAULT_REPOSITORY")
(PUNCT ")")
(PUNCT ",")
(PUNCT "(")
(LIT "show-response")
(PUNCT ",")
(KEYWORD None)
(PUNCT ",")
(LIT "display full response text from server")
(PUNCT ")")
(PUNCT "]")
(NEWLINE)
(ID "boolean_options")
(PUNCT "=")
(PUNCT "[")
(LIT "show-response")
(PUNCT "]")
(NEWLINE)
(KEYWORD def)
(ID "_get_rc_file")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Returns rc file path.")
(NEWLINE)
(KEYWORD return)
(ID "os")
(PUNCT ".")
(ID "path")
(PUNCT ".")
(ID "join")
(PUNCT "(")
(ID "os")
(PUNCT ".")
(ID "path")
(PUNCT ".")
(ID "expanduser")
(PUNCT "(")
(LIT "~")
(PUNCT ")")
(PUNCT ",")
(LIT ".pypirc")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "_store_pypirc")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "username")
(PUNCT ",")
(ID "password")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Creates a default .pypirc file.")
(NEWLINE)
(ID "rc")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "_get_rc_file")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(KEYWORD with)
(ID "os")
(PUNCT ".")
(ID "fdopen")
(PUNCT "(")
(ID "os")
(PUNCT ".")
(ID "open")
(PUNCT "(")
(ID "rc")
(PUNCT ",")
(ID "os")
(PUNCT ".")
(ID "O_CREAT")
(PUNCT "|")
(ID "os")
(PUNCT ".")
(ID "O_WRONLY")
(PUNCT ",")
(LIT 384)
(PUNCT ")")
(PUNCT ",")
(LIT "w")
(PUNCT ")")
(KEYWORD as)
(ID "f")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "f")
(PUNCT ".")
(ID "write")
(PUNCT "(")
(ID "DEFAULT_PYPIRC")
(PUNCT "%")
(PUNCT "(")
(ID "username")
(PUNCT ",")
(ID "password")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD def)
(ID "_read_pypirc")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Reads the .pypirc file.")
(NEWLINE)
(ID "rc")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "_get_rc_file")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(ID "os")
(PUNCT ".")
(ID "path")
(PUNCT ".")
(ID "exists")
(PUNCT "(")
(ID "rc")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "announce")
(PUNCT "(")
(LIT "Using PyPI login from %s")
(PUNCT "%")
(ID "rc")
(PUNCT ")")
(NEWLINE)
(ID "repository")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "repository")
(KEYWORD or)
(ID "self")
(PUNCT ".")
(ID "DEFAULT_REPOSITORY")
(NEWLINE)
(ID "realm")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "realm")
(KEYWORD or)
(ID "self")
(PUNCT ".")
(ID "DEFAULT_REALM")
(NEWLINE)
(ID "config")
(PUNCT "=")
(ID "ConfigParser")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "config")
(PUNCT ".")
(ID "read")
(PUNCT "(")
(ID "rc")
(PUNCT ")")
(NEWLINE)
(ID "sections")
(PUNCT "=")
(ID "config")
(PUNCT ".")
(ID "sections")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(LIT "distutils")
(KEYWORD in)
(ID "sections")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "index_servers")
(PUNCT "=")
(ID "config")
(PUNCT ".")
(ID "get")
(PUNCT "(")
(LIT "distutils")
(PUNCT ",")
(LIT "index-servers")
(PUNCT ")")
(NEWLINE)
(ID "_servers")
(PUNCT "=")
(PUNCT "[")
(ID "server")
(PUNCT ".")
(ID "strip")
(PUNCT "(")
(PUNCT ")")
(KEYWORD for)
(ID "server")
(KEYWORD in)
(ID "index_servers")
(PUNCT ".")
(ID "split")
(PUNCT "(")
(LIT "\n")
(PUNCT ")")
(KEYWORD if)
(ID "server")
(PUNCT ".")
(ID "strip")
(PUNCT "(")
(PUNCT ")")
(PUNCT "!=")
(LIT "")
(PUNCT "]")
(NEWLINE)
(KEYWORD if)
(ID "_servers")
(PUNCT "==")
(PUNCT "[")
(PUNCT "]")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(LIT "pypi")
(KEYWORD in)
(ID "sections")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "_servers")
(PUNCT "=")
(PUNCT "[")
(LIT "pypi")
(PUNCT "]")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(PUNCT "{")
(PUNCT "}")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD for)
(ID "server")
(KEYWORD in)
(ID "_servers")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "current")
(PUNCT "=")
(PUNCT "{")
(LIT "server")
(PUNCT ":")
(ID "server")
(PUNCT "}")
(NEWLINE)
(ID "current")
(PUNCT "[")
(LIT "username")
(PUNCT "]")
(PUNCT "=")
(ID "config")
(PUNCT ".")
(ID "get")
(PUNCT "(")
(ID "server")
(PUNCT ",")
(LIT "username")
(PUNCT ")")
(NEWLINE)
(KEYWORD for)
(ID "key")
(PUNCT ",")
(ID "default")
(KEYWORD in)
(PUNCT "(")
(PUNCT "(")
(LIT "repository")
(PUNCT ",")
(ID "self")
(PUNCT ".")
(ID "DEFAULT_REPOSITORY")
(PUNCT ")")
(PUNCT ",")
(PUNCT "(")
(LIT "realm")
(PUNCT ",")
(ID "self")
(PUNCT ".")
(ID "DEFAULT_REALM")
(PUNCT ")")
(PUNCT ",")
(PUNCT "(")
(LIT "password")
(PUNCT ",")
(KEYWORD None)
(PUNCT ")")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "config")
(PUNCT ".")
(ID "has_option")
(PUNCT "(")
(ID "server")
(PUNCT ",")
(ID "key")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "current")
(PUNCT "[")
(ID "key")
(PUNCT "]")
(PUNCT "=")
(ID "config")
(PUNCT ".")
(ID "get")
(PUNCT "(")
(ID "server")
(PUNCT ",")
(ID "key")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "current")
(PUNCT "[")
(ID "key")
(PUNCT "]")
(PUNCT "=")
(ID "default")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD if)
(PUNCT "(")
(ID "server")
(PUNCT "==")
(LIT "pypi")
(KEYWORD and)
(ID "repository")
(KEYWORD in)
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "DEFAULT_REPOSITORY")
(PUNCT ",")
(LIT "pypi")
(PUNCT ")")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "current")
(PUNCT "[")
(LIT "repository")
(PUNCT "]")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "DEFAULT_REPOSITORY")
(NEWLINE)
(KEYWORD return)
(ID "current")
(NEWLINE)
(DEDENT)
(KEYWORD if)
(PUNCT "(")
(ID "current")
(PUNCT "[")
(LIT "server")
(PUNCT "]")
(PUNCT "==")
(ID "repository")
(KEYWORD or)
(ID "current")
(PUNCT "[")
(LIT "repository")
(PUNCT "]")
(PUNCT "==")
(ID "repository")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(ID "current")
(NEWLINE)
(DEDENT)
(DEDENT)
(DEDENT)
(KEYWORD elif)
(LIT "server-login")
(KEYWORD in)
(ID "sections")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "server")
(PUNCT "=")
(LIT "server-login")
(NEWLINE)
(KEYWORD if)
(ID "config")
(PUNCT ".")
(ID "has_option")
(PUNCT "(")
(ID "server")
(PUNCT ",")
(LIT "repository")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "repository")
(PUNCT "=")
(ID "config")
(PUNCT ".")
(ID "get")
(PUNCT "(")
(ID "server")
(PUNCT ",")
(LIT "repository")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "repository")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "DEFAULT_REPOSITORY")
(NEWLINE)
(DEDENT)
(KEYWORD return)
(PUNCT "{")
(LIT "username")
(PUNCT ":")
(ID "config")
(PUNCT ".")
(ID "get")
(PUNCT "(")
(ID "server")
(PUNCT ",")
(LIT "username")
(PUNCT ")")
(PUNCT ",")
(LIT "password")
(PUNCT ":")
(ID "config")
(PUNCT ".")
(ID "get")
(PUNCT "(")
(ID "server")
(PUNCT ",")
(LIT "password")
(PUNCT ")")
(PUNCT ",")
(LIT "repository")
(PUNCT ":")
(ID "repository")
(PUNCT ",")
(LIT "server")
(PUNCT ":")
(ID "server")
(PUNCT ",")
(LIT "realm")
(PUNCT ":")
(ID "self")
(PUNCT ".")
(ID "DEFAULT_REALM")
(PUNCT "}")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD return)
(PUNCT "{")
(PUNCT "}")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "_read_pypi_response")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "response")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Read and decode a PyPI HTTP response.")
(NEWLINE)
(KEYWORD import)
(ID "cgi")
(NEWLINE)
(ID "content_type")
(PUNCT "=")
(ID "response")
(PUNCT ".")
(ID "getheader")
(PUNCT "(")
(LIT "content-type")
(PUNCT ",")
(LIT "text/plain")
(PUNCT ")")
(NEWLINE)
(ID "encoding")
(PUNCT "=")
(ID "cgi")
(PUNCT ".")
(ID "parse_header")
(PUNCT "(")
(ID "content_type")
(PUNCT ")")
(PUNCT "[")
(LIT 1)
(PUNCT "]")
(PUNCT ".")
(ID "get")
(PUNCT "(")
(LIT "charset")
(PUNCT ",")
(LIT "ascii")
(PUNCT ")")
(NEWLINE)
(KEYWORD return)
(ID "response")
(PUNCT ".")
(ID "read")
(PUNCT "(")
(PUNCT ")")
(PUNCT ".")
(ID "decode")
(PUNCT "(")
(ID "encoding")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "initialize_options")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Initialize options.")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "repository")
(PUNCT "=")
(KEYWORD None)
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "realm")
(PUNCT "=")
(KEYWORD None)
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "show_response")
(PUNCT "=")
(LIT 0)
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "finalize_options")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Finalizes options.")
(NEWLINE)
(KEYWORD if)
(ID "self")
(PUNCT ".")
(ID "repository")
(KEYWORD is)
(KEYWORD None)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "repository")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "DEFAULT_REPOSITORY")
(NEWLINE)
(DEDENT)
(KEYWORD if)
(ID "self")
(PUNCT ".")
(ID "realm")
(KEYWORD is)
(KEYWORD None)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "realm")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "DEFAULT_REALM")
(NEWLINE)
(DEDENT)
(DEDENT)
(DEDENT)
(ENDMARKER)