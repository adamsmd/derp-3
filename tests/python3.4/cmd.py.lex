(LIT "A generic class to build line-oriented command interpreters.\n\nInterpreters constructed with this class obey the following conventions:\n\n1. End of file on input is processed as the command 'EOF'.\n2. A command is parsed out of each line by collecting the prefix composed\n   of characters in the identchars member.\n3. A command `foo' is dispatched to a method 'do_foo()'; the do_ method\n   is passed a single argument consisting of the remainder of the line.\n4. Typing an empty line repeats the last command.  (Actually, it calls the\n   method `emptyline', which may be overridden in a subclass.)\n5. There is a predefined `help' method.  Given an argument `topic', it\n   calls the command `help_topic'.  With no arguments, it lists all topics\n   with defined help_ functions, broken into up to three topics; documented\n   commands, miscellaneous help topics, and undocumented commands.\n6. The command '?' is a synonym for `help'.  The command '!' is a synonym\n   for `shell', if a do_shell method exists.\n7. If completion is enabled, completing commands will be done automatically,\n   and completing of commands args is done by calling complete_foo() with\n   arguments text, line, begidx, endidx.  text is string we are matching\n   against, all returned matches must begin with it.  line is the current\n   input line (lstripped), begidx and endidx are the beginning and end\n   indexes of the text being matched, which could be used to provide\n   different completion depending upon which position the argument is in.\n\nThe `default' method may be overridden to intercept commands for which there\nis no do_ method.\n\nThe `completedefault' method may be overridden to intercept completions for\ncommands that have no complete_ method.\n\nThe data member `self.ruler' sets the character used to draw separator lines\nin the help messages.  If empty, no ruler line is drawn.  It defaults to \"=\".\n\nIf the value of `self.intro' is nonempty when the cmdloop method is called,\nit is printed out on interpreter startup.  This value may be overridden\nvia an optional argument to the cmdloop() method.\n\nThe data members `self.doc_header', `self.misc_header', and\n`self.undoc_header' set the headers used for the help function's\nlistings of documented functions, miscellaneous topics, and undocumented\nfunctions respectively.\n")
(NEWLINE)
(KEYWORD import)
(ID "string")
(PUNCT ",")
(ID "sys")
(NEWLINE)
(ID "__all__")
(PUNCT "=")
(PUNCT "[")
(LIT "Cmd")
(PUNCT "]")
(NEWLINE)
(ID "PROMPT")
(PUNCT "=")
(LIT "(Cmd) ")
(NEWLINE)
(ID "IDENTCHARS")
(PUNCT "=")
(ID "string")
(PUNCT ".")
(ID "ascii_letters")
(PUNCT "+")
(ID "string")
(PUNCT ".")
(ID "digits")
(PUNCT "+")
(LIT "_")
(NEWLINE)
(KEYWORD class)
(ID "Cmd")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "A simple framework for writing line-oriented command interpreters.\n\n    These are often useful for test harnesses, administrative tools, and\n    prototypes that will later be wrapped in a more sophisticated interface.\n\n    A Cmd instance or subclass instance is a line-oriented interpreter\n    framework.  There is no good reason to instantiate Cmd itself; rather,\n    it's useful as a superclass of an interpreter class you define yourself\n    in order to inherit Cmd's methods and encapsulate action methods.\n\n    ")
(NEWLINE)
(ID "prompt")
(PUNCT "=")
(ID "PROMPT")
(NEWLINE)
(ID "identchars")
(PUNCT "=")
(ID "IDENTCHARS")
(NEWLINE)
(ID "ruler")
(PUNCT "=")
(LIT "=")
(NEWLINE)
(ID "lastcmd")
(PUNCT "=")
(LIT "")
(NEWLINE)
(ID "intro")
(PUNCT "=")
(KEYWORD None)
(NEWLINE)
(ID "doc_leader")
(PUNCT "=")
(LIT "")
(NEWLINE)
(ID "doc_header")
(PUNCT "=")
(LIT "Documented commands (type help <topic>):")
(NEWLINE)
(ID "misc_header")
(PUNCT "=")
(LIT "Miscellaneous help topics:")
(NEWLINE)
(ID "undoc_header")
(PUNCT "=")
(LIT "Undocumented commands:")
(NEWLINE)
(ID "nohelp")
(PUNCT "=")
(LIT "*** No help on %s")
(NEWLINE)
(ID "use_rawinput")
(PUNCT "=")
(LIT 1)
(NEWLINE)
(KEYWORD def)
(ID "__init__")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "completekey")
(PUNCT "=")
(LIT "tab")
(PUNCT ",")
(ID "stdin")
(PUNCT "=")
(KEYWORD None)
(PUNCT ",")
(ID "stdout")
(PUNCT "=")
(KEYWORD None)
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Instantiate a line-oriented interpreter framework.\n\n        The optional argument 'completekey' is the readline name of a\n        completion key; it defaults to the Tab key. If completekey is\n        not None and the readline module is available, command completion\n        is done automatically. The optional arguments stdin and stdout\n        specify alternate input and output file objects; if not specified,\n        sys.stdin and sys.stdout are used.\n\n        ")
(NEWLINE)
(KEYWORD if)
(ID "stdin")
(KEYWORD is)
(KEYWORD not)
(KEYWORD None)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "stdin")
(PUNCT "=")
(ID "stdin")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "stdin")
(PUNCT "=")
(ID "sys")
(PUNCT ".")
(ID "stdin")
(NEWLINE)
(DEDENT)
(KEYWORD if)
(ID "stdout")
(KEYWORD is)
(KEYWORD not)
(KEYWORD None)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "stdout")
(PUNCT "=")
(ID "stdout")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "stdout")
(PUNCT "=")
(ID "sys")
(PUNCT ".")
(ID "stdout")
(NEWLINE)
(DEDENT)
(ID "self")
(PUNCT ".")
(ID "cmdqueue")
(PUNCT "=")
(PUNCT "[")
(PUNCT "]")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "completekey")
(PUNCT "=")
(ID "completekey")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "cmdloop")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "intro")
(PUNCT "=")
(KEYWORD None)
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Repeatedly issue a prompt, accept input, parse an initial prefix\n        off the received input, and dispatch to action methods, passing them\n        the remainder of the line as argument.\n\n        ")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "preloop")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(ID "self")
(PUNCT ".")
(ID "use_rawinput")
(KEYWORD and)
(ID "self")
(PUNCT ".")
(ID "completekey")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD try)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD import)
(ID "readline")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "old_completer")
(PUNCT "=")
(ID "readline")
(PUNCT ".")
(ID "get_completer")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "readline")
(PUNCT ".")
(ID "set_completer")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "complete")
(PUNCT ")")
(NEWLINE)
(ID "readline")
(PUNCT ".")
(ID "parse_and_bind")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "completekey")
(PUNCT "+")
(LIT ": complete")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD except)
(ID "ImportError")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD pass)
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD try)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "intro")
(KEYWORD is)
(KEYWORD not)
(KEYWORD None)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "intro")
(PUNCT "=")
(ID "intro")
(NEWLINE)
(DEDENT)
(KEYWORD if)
(ID "self")
(PUNCT ".")
(ID "intro")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "stdout")
(PUNCT ".")
(ID "write")
(PUNCT "(")
(ID "str")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "intro")
(PUNCT ")")
(PUNCT "+")
(LIT "\n")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "stop")
(PUNCT "=")
(KEYWORD None)
(NEWLINE)
(KEYWORD while)
(KEYWORD not)
(ID "stop")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "self")
(PUNCT ".")
(ID "cmdqueue")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "line")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "cmdqueue")
(PUNCT ".")
(ID "pop")
(PUNCT "(")
(LIT 0)
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "self")
(PUNCT ".")
(ID "use_rawinput")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD try)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "line")
(PUNCT "=")
(ID "input")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "prompt")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD except)
(ID "EOFError")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "line")
(PUNCT "=")
(LIT "EOF")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "stdout")
(PUNCT ".")
(ID "write")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "prompt")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "stdout")
(PUNCT ".")
(ID "flush")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "line")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "stdin")
(PUNCT ".")
(ID "readline")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(KEYWORD not)
(ID "len")
(PUNCT "(")
(ID "line")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "line")
(PUNCT "=")
(LIT "EOF")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "line")
(PUNCT "=")
(ID "line")
(PUNCT ".")
(ID "rstrip")
(PUNCT "(")
(LIT "\r\n")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(DEDENT)
(ID "line")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "precmd")
(PUNCT "(")
(ID "line")
(PUNCT ")")
(NEWLINE)
(ID "stop")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "onecmd")
(PUNCT "(")
(ID "line")
(PUNCT ")")
(NEWLINE)
(ID "stop")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "postcmd")
(PUNCT "(")
(ID "stop")
(PUNCT ",")
(ID "line")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "self")
(PUNCT ".")
(ID "postloop")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD finally)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "self")
(PUNCT ".")
(ID "use_rawinput")
(KEYWORD and)
(ID "self")
(PUNCT ".")
(ID "completekey")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD try)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD import)
(ID "readline")
(NEWLINE)
(ID "readline")
(PUNCT ".")
(ID "set_completer")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "old_completer")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD except)
(ID "ImportError")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD pass)
(NEWLINE)
(DEDENT)
(DEDENT)
(DEDENT)
(DEDENT)
(KEYWORD def)
(ID "precmd")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "line")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Hook method executed just before the command line is\n        interpreted, but after the input prompt is generated and issued.\n\n        ")
(NEWLINE)
(KEYWORD return)
(ID "line")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "postcmd")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "stop")
(PUNCT ",")
(ID "line")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Hook method executed just after a command dispatch is finished.")
(NEWLINE)
(KEYWORD return)
(ID "stop")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "preloop")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Hook method executed once when the cmdloop() method is called.")
(NEWLINE)
(KEYWORD pass)
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "postloop")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Hook method executed once when the cmdloop() method is about to\n        return.\n\n        ")
(NEWLINE)
(KEYWORD pass)
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "parseline")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "line")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Parse the line into a command name and a string containing\n        the arguments.  Returns a tuple containing (command, args, line).\n        'command' and 'args' may be None if the line couldn't be parsed.\n        ")
(NEWLINE)
(ID "line")
(PUNCT "=")
(ID "line")
(PUNCT ".")
(ID "strip")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(KEYWORD not)
(ID "line")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(KEYWORD None)
(PUNCT ",")
(KEYWORD None)
(PUNCT ",")
(ID "line")
(NEWLINE)
(DEDENT)
(KEYWORD elif)
(ID "line")
(PUNCT "[")
(LIT 0)
(PUNCT "]")
(PUNCT "==")
(LIT "?")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "line")
(PUNCT "=")
(LIT "help ")
(PUNCT "+")
(ID "line")
(PUNCT "[")
(LIT 1)
(PUNCT ":")
(PUNCT "]")
(NEWLINE)
(DEDENT)
(KEYWORD elif)
(ID "line")
(PUNCT "[")
(LIT 0)
(PUNCT "]")
(PUNCT "==")
(LIT "!")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "hasattr")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(LIT "do_shell")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "line")
(PUNCT "=")
(LIT "shell ")
(PUNCT "+")
(ID "line")
(PUNCT "[")
(LIT 1)
(PUNCT ":")
(PUNCT "]")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(KEYWORD None)
(PUNCT ",")
(KEYWORD None)
(PUNCT ",")
(ID "line")
(NEWLINE)
(DEDENT)
(DEDENT)
(ID "i")
(PUNCT ",")
(ID "n")
(PUNCT "=")
(LIT 0)
(PUNCT ",")
(ID "len")
(PUNCT "(")
(ID "line")
(PUNCT ")")
(NEWLINE)
(KEYWORD while)
(ID "i")
(PUNCT "<")
(ID "n")
(KEYWORD and)
(ID "line")
(PUNCT "[")
(ID "i")
(PUNCT "]")
(KEYWORD in)
(ID "self")
(PUNCT ".")
(ID "identchars")
(PUNCT ":")
(ID "i")
(PUNCT "=")
(ID "i")
(PUNCT "+")
(LIT 1)
(NEWLINE)
(ID "cmd")
(PUNCT ",")
(ID "arg")
(PUNCT "=")
(ID "line")
(PUNCT "[")
(PUNCT ":")
(ID "i")
(PUNCT "]")
(PUNCT ",")
(ID "line")
(PUNCT "[")
(ID "i")
(PUNCT ":")
(PUNCT "]")
(PUNCT ".")
(ID "strip")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(KEYWORD return)
(ID "cmd")
(PUNCT ",")
(ID "arg")
(PUNCT ",")
(ID "line")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "onecmd")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "line")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Interpret the argument as though it had been typed in response\n        to the prompt.\n\n        This may be overridden, but should not normally need to be;\n        see the precmd() and postcmd() methods for useful execution hooks.\n        The return value is a flag indicating whether interpretation of\n        commands by the interpreter should stop.\n\n        ")
(NEWLINE)
(ID "cmd")
(PUNCT ",")
(ID "arg")
(PUNCT ",")
(ID "line")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "parseline")
(PUNCT "(")
(ID "line")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(KEYWORD not)
(ID "line")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(ID "self")
(PUNCT ".")
(ID "emptyline")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD if)
(ID "cmd")
(KEYWORD is)
(KEYWORD None)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(ID "self")
(PUNCT ".")
(ID "default")
(PUNCT "(")
(ID "line")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "self")
(PUNCT ".")
(ID "lastcmd")
(PUNCT "=")
(ID "line")
(NEWLINE)
(KEYWORD if)
(ID "line")
(PUNCT "==")
(LIT "EOF")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "lastcmd")
(PUNCT "=")
(LIT "")
(NEWLINE)
(DEDENT)
(KEYWORD if)
(ID "cmd")
(PUNCT "==")
(LIT "")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(ID "self")
(PUNCT ".")
(ID "default")
(PUNCT "(")
(ID "line")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD try)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "func")
(PUNCT "=")
(ID "getattr")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(LIT "do_")
(PUNCT "+")
(ID "cmd")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD except)
(ID "AttributeError")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(ID "self")
(PUNCT ".")
(ID "default")
(PUNCT "(")
(ID "line")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD return)
(ID "func")
(PUNCT "(")
(ID "arg")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD def)
(ID "emptyline")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Called when an empty line is entered in response to the prompt.\n\n        If this method is not overridden, it repeats the last nonempty\n        command entered.\n\n        ")
(NEWLINE)
(KEYWORD if)
(ID "self")
(PUNCT ".")
(ID "lastcmd")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(ID "self")
(PUNCT ".")
(ID "onecmd")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "lastcmd")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD def)
(ID "default")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "line")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Called on an input line when the command prefix is not recognized.\n\n        If this method is not overridden, it prints an error message and\n        returns.\n\n        ")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "stdout")
(PUNCT ".")
(ID "write")
(PUNCT "(")
(LIT "*** Unknown syntax: %s\n")
(PUNCT "%")
(ID "line")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "completedefault")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(PUNCT "*")
(ID "ignored")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Method called to complete an input line when no command-specific\n        complete_*() method is available.\n\n        By default, it returns an empty list.\n\n        ")
(NEWLINE)
(KEYWORD return)
(PUNCT "[")
(PUNCT "]")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "completenames")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "text")
(PUNCT ",")
(PUNCT "*")
(ID "ignored")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "dotext")
(PUNCT "=")
(LIT "do_")
(PUNCT "+")
(ID "text")
(NEWLINE)
(KEYWORD return)
(PUNCT "[")
(ID "a")
(PUNCT "[")
(LIT 3)
(PUNCT ":")
(PUNCT "]")
(KEYWORD for)
(ID "a")
(KEYWORD in)
(ID "self")
(PUNCT ".")
(ID "get_names")
(PUNCT "(")
(PUNCT ")")
(KEYWORD if)
(ID "a")
(PUNCT ".")
(ID "startswith")
(PUNCT "(")
(ID "dotext")
(PUNCT ")")
(PUNCT "]")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "complete")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "text")
(PUNCT ",")
(ID "state")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Return the next possible completion for 'text'.\n\n        If a command has not been entered, then complete against command list.\n        Otherwise try to call complete_<command> to get list of completions.\n        ")
(NEWLINE)
(KEYWORD if)
(ID "state")
(PUNCT "==")
(LIT 0)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD import)
(ID "readline")
(NEWLINE)
(ID "origline")
(PUNCT "=")
(ID "readline")
(PUNCT ".")
(ID "get_line_buffer")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "line")
(PUNCT "=")
(ID "origline")
(PUNCT ".")
(ID "lstrip")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "stripped")
(PUNCT "=")
(ID "len")
(PUNCT "(")
(ID "origline")
(PUNCT ")")
(PUNCT "-")
(ID "len")
(PUNCT "(")
(ID "line")
(PUNCT ")")
(NEWLINE)
(ID "begidx")
(PUNCT "=")
(ID "readline")
(PUNCT ".")
(ID "get_begidx")
(PUNCT "(")
(PUNCT ")")
(PUNCT "-")
(ID "stripped")
(NEWLINE)
(ID "endidx")
(PUNCT "=")
(ID "readline")
(PUNCT ".")
(ID "get_endidx")
(PUNCT "(")
(PUNCT ")")
(PUNCT "-")
(ID "stripped")
(NEWLINE)
(KEYWORD if)
(ID "begidx")
(PUNCT ">")
(LIT 0)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "cmd")
(PUNCT ",")
(ID "args")
(PUNCT ",")
(ID "foo")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "parseline")
(PUNCT "(")
(ID "line")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(ID "cmd")
(PUNCT "==")
(LIT "")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "compfunc")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "completedefault")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD try)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "compfunc")
(PUNCT "=")
(ID "getattr")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(LIT "complete_")
(PUNCT "+")
(ID "cmd")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD except)
(ID "AttributeError")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "compfunc")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "completedefault")
(NEWLINE)
(DEDENT)
(DEDENT)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "compfunc")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "completenames")
(NEWLINE)
(DEDENT)
(ID "self")
(PUNCT ".")
(ID "completion_matches")
(PUNCT "=")
(ID "compfunc")
(PUNCT "(")
(ID "text")
(PUNCT ",")
(ID "line")
(PUNCT ",")
(ID "begidx")
(PUNCT ",")
(ID "endidx")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD try)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(ID "self")
(PUNCT ".")
(ID "completion_matches")
(PUNCT "[")
(ID "state")
(PUNCT "]")
(NEWLINE)
(DEDENT)
(KEYWORD except)
(ID "IndexError")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(KEYWORD None)
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD def)
(ID "get_names")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(ID "dir")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "__class__")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "complete_help")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(PUNCT "*")
(ID "args")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "commands")
(PUNCT "=")
(ID "set")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "completenames")
(PUNCT "(")
(PUNCT "*")
(ID "args")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(ID "topics")
(PUNCT "=")
(ID "set")
(PUNCT "(")
(ID "a")
(PUNCT "[")
(LIT 5)
(PUNCT ":")
(PUNCT "]")
(KEYWORD for)
(ID "a")
(KEYWORD in)
(ID "self")
(PUNCT ".")
(ID "get_names")
(PUNCT "(")
(PUNCT ")")
(KEYWORD if)
(ID "a")
(PUNCT ".")
(ID "startswith")
(PUNCT "(")
(LIT "help_")
(PUNCT "+")
(ID "args")
(PUNCT "[")
(LIT 0)
(PUNCT "]")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(KEYWORD return)
(ID "list")
(PUNCT "(")
(ID "commands")
(PUNCT "|")
(ID "topics")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "do_help")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "arg")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "List available commands with \"help\" or detailed help with \"help cmd\".")
(NEWLINE)
(KEYWORD if)
(ID "arg")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD try)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "func")
(PUNCT "=")
(ID "getattr")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(LIT "help_")
(PUNCT "+")
(ID "arg")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD except)
(ID "AttributeError")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD try)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "doc")
(PUNCT "=")
(ID "getattr")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(LIT "do_")
(PUNCT "+")
(ID "arg")
(PUNCT ")")
(PUNCT ".")
(ID "__doc__")
(NEWLINE)
(KEYWORD if)
(ID "doc")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "stdout")
(PUNCT ".")
(ID "write")
(PUNCT "(")
(LIT "%s\n")
(PUNCT "%")
(ID "str")
(PUNCT "(")
(ID "doc")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(KEYWORD return)
(NEWLINE)
(DEDENT)
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
(ID "stdout")
(PUNCT ".")
(ID "write")
(PUNCT "(")
(LIT "%s\n")
(PUNCT "%")
(ID "str")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "nohelp")
(PUNCT "%")
(PUNCT "(")
(ID "arg")
(PUNCT ",")
(PUNCT ")")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(KEYWORD return)
(NEWLINE)
(DEDENT)
(ID "func")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "names")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "get_names")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "cmds_doc")
(PUNCT "=")
(PUNCT "[")
(PUNCT "]")
(NEWLINE)
(ID "cmds_undoc")
(PUNCT "=")
(PUNCT "[")
(PUNCT "]")
(NEWLINE)
(ID "help")
(PUNCT "=")
(PUNCT "{")
(PUNCT "}")
(NEWLINE)
(KEYWORD for)
(ID "name")
(KEYWORD in)
(ID "names")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "name")
(PUNCT "[")
(PUNCT ":")
(LIT 5)
(PUNCT "]")
(PUNCT "==")
(LIT "help_")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "help")
(PUNCT "[")
(ID "name")
(PUNCT "[")
(LIT 5)
(PUNCT ":")
(PUNCT "]")
(PUNCT "]")
(PUNCT "=")
(LIT 1)
(NEWLINE)
(DEDENT)
(DEDENT)
(ID "names")
(PUNCT ".")
(ID "sort")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "prevname")
(PUNCT "=")
(LIT "")
(NEWLINE)
(KEYWORD for)
(ID "name")
(KEYWORD in)
(ID "names")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "name")
(PUNCT "[")
(PUNCT ":")
(LIT 3)
(PUNCT "]")
(PUNCT "==")
(LIT "do_")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "name")
(PUNCT "==")
(ID "prevname")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD continue)
(NEWLINE)
(DEDENT)
(ID "prevname")
(PUNCT "=")
(ID "name")
(NEWLINE)
(ID "cmd")
(PUNCT "=")
(ID "name")
(PUNCT "[")
(LIT 3)
(PUNCT ":")
(PUNCT "]")
(NEWLINE)
(KEYWORD if)
(ID "cmd")
(KEYWORD in)
(ID "help")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "cmds_doc")
(PUNCT ".")
(ID "append")
(PUNCT "(")
(ID "cmd")
(PUNCT ")")
(NEWLINE)
(KEYWORD del)
(ID "help")
(PUNCT "[")
(ID "cmd")
(PUNCT "]")
(NEWLINE)
(DEDENT)
(KEYWORD elif)
(ID "getattr")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "name")
(PUNCT ")")
(PUNCT ".")
(ID "__doc__")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "cmds_doc")
(PUNCT ".")
(ID "append")
(PUNCT "(")
(ID "cmd")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "cmds_undoc")
(PUNCT ".")
(ID "append")
(PUNCT "(")
(ID "cmd")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(DEDENT)
(ID "self")
(PUNCT ".")
(ID "stdout")
(PUNCT ".")
(ID "write")
(PUNCT "(")
(LIT "%s\n")
(PUNCT "%")
(ID "str")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "doc_leader")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "print_topics")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "doc_header")
(PUNCT ",")
(ID "cmds_doc")
(PUNCT ",")
(LIT 15)
(PUNCT ",")
(LIT 80)
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "print_topics")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "misc_header")
(PUNCT ",")
(ID "list")
(PUNCT "(")
(ID "help")
(PUNCT ".")
(ID "keys")
(PUNCT "(")
(PUNCT ")")
(PUNCT ")")
(PUNCT ",")
(LIT 15)
(PUNCT ",")
(LIT 80)
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "print_topics")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "undoc_header")
(PUNCT ",")
(ID "cmds_undoc")
(PUNCT ",")
(LIT 15)
(PUNCT ",")
(LIT 80)
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD def)
(ID "print_topics")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "header")
(PUNCT ",")
(ID "cmds")
(PUNCT ",")
(ID "cmdlen")
(PUNCT ",")
(ID "maxcol")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "cmds")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "stdout")
(PUNCT ".")
(ID "write")
(PUNCT "(")
(LIT "%s\n")
(PUNCT "%")
(ID "str")
(PUNCT "(")
(ID "header")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(ID "self")
(PUNCT ".")
(ID "ruler")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "stdout")
(PUNCT ".")
(ID "write")
(PUNCT "(")
(LIT "%s\n")
(PUNCT "%")
(ID "str")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "ruler")
(PUNCT "*")
(ID "len")
(PUNCT "(")
(ID "header")
(PUNCT ")")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "self")
(PUNCT ".")
(ID "columnize")
(PUNCT "(")
(ID "cmds")
(PUNCT ",")
(ID "maxcol")
(PUNCT "-")
(LIT 1)
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "stdout")
(PUNCT ".")
(ID "write")
(PUNCT "(")
(LIT "\n")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD def)
(ID "columnize")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "list")
(PUNCT ",")
(ID "displaywidth")
(PUNCT "=")
(LIT 80)
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Display a list of strings as a compact set of columns.\n\n        Each column is only as wide as necessary.\n        Columns are separated by two spaces (one was not legible enough).\n        ")
(NEWLINE)
(KEYWORD if)
(KEYWORD not)
(ID "list")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "stdout")
(PUNCT ".")
(ID "write")
(PUNCT "(")
(LIT "<empty>\n")
(PUNCT ")")
(NEWLINE)
(KEYWORD return)
(NEWLINE)
(DEDENT)
(ID "nonstrings")
(PUNCT "=")
(PUNCT "[")
(ID "i")
(KEYWORD for)
(ID "i")
(KEYWORD in)
(ID "range")
(PUNCT "(")
(ID "len")
(PUNCT "(")
(ID "list")
(PUNCT ")")
(PUNCT ")")
(KEYWORD if)
(KEYWORD not)
(ID "isinstance")
(PUNCT "(")
(ID "list")
(PUNCT "[")
(ID "i")
(PUNCT "]")
(PUNCT ",")
(ID "str")
(PUNCT ")")
(PUNCT "]")
(NEWLINE)
(KEYWORD if)
(ID "nonstrings")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD raise)
(ID "TypeError")
(PUNCT "(")
(LIT "list[i] not a string for i in %s")
(PUNCT "%")
(LIT ", ")
(PUNCT ".")
(ID "join")
(PUNCT "(")
(ID "map")
(PUNCT "(")
(ID "str")
(PUNCT ",")
(ID "nonstrings")
(PUNCT ")")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "size")
(PUNCT "=")
(ID "len")
(PUNCT "(")
(ID "list")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(ID "size")
(PUNCT "==")
(LIT 1)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "stdout")
(PUNCT ".")
(ID "write")
(PUNCT "(")
(LIT "%s\n")
(PUNCT "%")
(ID "str")
(PUNCT "(")
(ID "list")
(PUNCT "[")
(LIT 0)
(PUNCT "]")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(KEYWORD return)
(NEWLINE)
(DEDENT)
(KEYWORD for)
(ID "nrows")
(KEYWORD in)
(ID "range")
(PUNCT "(")
(LIT 1)
(PUNCT ",")
(ID "len")
(PUNCT "(")
(ID "list")
(PUNCT ")")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "ncols")
(PUNCT "=")
(PUNCT "(")
(ID "size")
(PUNCT "+")
(ID "nrows")
(PUNCT "-")
(LIT 1)
(PUNCT ")")
(PUNCT "//")
(ID "nrows")
(NEWLINE)
(ID "colwidths")
(PUNCT "=")
(PUNCT "[")
(PUNCT "]")
(NEWLINE)
(ID "totwidth")
(PUNCT "=")
(PUNCT "-")
(LIT 2)
(NEWLINE)
(KEYWORD for)
(ID "col")
(KEYWORD in)
(ID "range")
(PUNCT "(")
(ID "ncols")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "colwidth")
(PUNCT "=")
(LIT 0)
(NEWLINE)
(KEYWORD for)
(ID "row")
(KEYWORD in)
(ID "range")
(PUNCT "(")
(ID "nrows")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "i")
(PUNCT "=")
(ID "row")
(PUNCT "+")
(ID "nrows")
(PUNCT "*")
(ID "col")
(NEWLINE)
(KEYWORD if)
(ID "i")
(PUNCT ">=")
(ID "size")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD break)
(NEWLINE)
(DEDENT)
(ID "x")
(PUNCT "=")
(ID "list")
(PUNCT "[")
(ID "i")
(PUNCT "]")
(NEWLINE)
(ID "colwidth")
(PUNCT "=")
(ID "max")
(PUNCT "(")
(ID "colwidth")
(PUNCT ",")
(ID "len")
(PUNCT "(")
(ID "x")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "colwidths")
(PUNCT ".")
(ID "append")
(PUNCT "(")
(ID "colwidth")
(PUNCT ")")
(NEWLINE)
(ID "totwidth")
(PUNCT "+=")
(ID "colwidth")
(PUNCT "+")
(LIT 2)
(NEWLINE)
(KEYWORD if)
(ID "totwidth")
(PUNCT ">")
(ID "displaywidth")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD break)
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD if)
(ID "totwidth")
(PUNCT "<=")
(ID "displaywidth")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD break)
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "nrows")
(PUNCT "=")
(ID "len")
(PUNCT "(")
(ID "list")
(PUNCT ")")
(NEWLINE)
(ID "ncols")
(PUNCT "=")
(LIT 1)
(NEWLINE)
(ID "colwidths")
(PUNCT "=")
(PUNCT "[")
(LIT 0)
(PUNCT "]")
(NEWLINE)
(DEDENT)
(KEYWORD for)
(ID "row")
(KEYWORD in)
(ID "range")
(PUNCT "(")
(ID "nrows")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "texts")
(PUNCT "=")
(PUNCT "[")
(PUNCT "]")
(NEWLINE)
(KEYWORD for)
(ID "col")
(KEYWORD in)
(ID "range")
(PUNCT "(")
(ID "ncols")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "i")
(PUNCT "=")
(ID "row")
(PUNCT "+")
(ID "nrows")
(PUNCT "*")
(ID "col")
(NEWLINE)
(KEYWORD if)
(ID "i")
(PUNCT ">=")
(ID "size")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "x")
(PUNCT "=")
(LIT "")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "x")
(PUNCT "=")
(ID "list")
(PUNCT "[")
(ID "i")
(PUNCT "]")
(NEWLINE)
(DEDENT)
(ID "texts")
(PUNCT ".")
(ID "append")
(PUNCT "(")
(ID "x")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD while)
(ID "texts")
(KEYWORD and)
(KEYWORD not)
(ID "texts")
(PUNCT "[")
(PUNCT "-")
(LIT 1)
(PUNCT "]")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD del)
(ID "texts")
(PUNCT "[")
(PUNCT "-")
(LIT 1)
(PUNCT "]")
(NEWLINE)
(DEDENT)
(KEYWORD for)
(ID "col")
(KEYWORD in)
(ID "range")
(PUNCT "(")
(ID "len")
(PUNCT "(")
(ID "texts")
(PUNCT ")")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "texts")
(PUNCT "[")
(ID "col")
(PUNCT "]")
(PUNCT "=")
(ID "texts")
(PUNCT "[")
(ID "col")
(PUNCT "]")
(PUNCT ".")
(ID "ljust")
(PUNCT "(")
(ID "colwidths")
(PUNCT "[")
(ID "col")
(PUNCT "]")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "self")
(PUNCT ".")
(ID "stdout")
(PUNCT ".")
(ID "write")
(PUNCT "(")
(LIT "%s\n")
(PUNCT "%")
(ID "str")
(PUNCT "(")
(LIT "  ")
(PUNCT ".")
(ID "join")
(PUNCT "(")
(ID "texts")
(PUNCT ")")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(DEDENT)
(ENDMARKER)
