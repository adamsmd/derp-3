(LIT "CallTips.py - An IDLE Extension to Jog Your Memory\n\nCall Tips are floating windows which display function, class, and method\nparameter and docstring information when you type an opening parenthesis, and\nwhich disappear when you type a closing parenthesis.\n\n")
(NEWLINE)
(KEYWORD import)
(ID "__main__")
(NEWLINE)
(KEYWORD import)
(ID "inspect")
(NEWLINE)
(KEYWORD import)
(ID "re")
(NEWLINE)
(KEYWORD import)
(ID "sys")
(NEWLINE)
(KEYWORD import)
(ID "textwrap")
(NEWLINE)
(KEYWORD import)
(ID "types")
(NEWLINE)
(KEYWORD from)
(ID "idlelib")
(KEYWORD import)
(ID "CallTipWindow")
(NEWLINE)
(KEYWORD from)
(ID "idlelib")
(PUNCT ".")
(ID "HyperParser")
(KEYWORD import)
(ID "HyperParser")
(NEWLINE)
(KEYWORD class)
(ID "CallTips")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "menudefs")
(PUNCT "=")
(PUNCT "[")
(PUNCT "(")
(LIT "edit")
(PUNCT ",")
(PUNCT "[")
(PUNCT "(")
(LIT "Show call tip")
(PUNCT ",")
(LIT "<<force-open-calltip>>")
(PUNCT ")")
(PUNCT ",")
(PUNCT "]")
(PUNCT ")")
(PUNCT "]")
(NEWLINE)
(KEYWORD def)
(ID "__init__")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "editwin")
(PUNCT "=")
(KEYWORD None)
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "editwin")
(KEYWORD is)
(KEYWORD None)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "editwin")
(PUNCT "=")
(KEYWORD None)
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "editwin")
(PUNCT "=")
(ID "editwin")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "text")
(PUNCT "=")
(ID "editwin")
(PUNCT ".")
(ID "text")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "active_calltip")
(PUNCT "=")
(KEYWORD None)
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_calltip_window")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "_make_tk_calltip_window")
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
(ID "self")
(PUNCT ".")
(ID "_calltip_window")
(PUNCT "=")
(KEYWORD None)
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "_make_tk_calltip_window")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(ID "CallTipWindow")
(PUNCT ".")
(ID "CallTip")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "text")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "_remove_calltip_window")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "event")
(PUNCT "=")
(KEYWORD None)
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "self")
(PUNCT ".")
(ID "active_calltip")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "active_calltip")
(PUNCT ".")
(ID "hidetip")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "active_calltip")
(PUNCT "=")
(KEYWORD None)
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD def)
(ID "force_open_calltip_event")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "event")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "The user selected the menu entry or hotkey, open the tip.")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "open_calltip")
(PUNCT "(")
(KEYWORD True)
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "try_open_calltip_event")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "event")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Happens when it would be nice to open a CallTip, but not really\n        necessary, for example after an opening bracket, so function calls\n        won't be made.\n        ")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "open_calltip")
(PUNCT "(")
(KEYWORD False)
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "refresh_calltip_event")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "event")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "self")
(PUNCT ".")
(ID "active_calltip")
(KEYWORD and)
(ID "self")
(PUNCT ".")
(ID "active_calltip")
(PUNCT ".")
(ID "is_active")
(PUNCT "(")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "open_calltip")
(PUNCT "(")
(KEYWORD False)
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD def)
(ID "open_calltip")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "evalfuncs")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "_remove_calltip_window")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "hp")
(PUNCT "=")
(ID "HyperParser")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "editwin")
(PUNCT ",")
(LIT "insert")
(PUNCT ")")
(NEWLINE)
(ID "sur_paren")
(PUNCT "=")
(ID "hp")
(PUNCT ".")
(ID "get_surrounding_brackets")
(PUNCT "(")
(LIT "(")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(KEYWORD not)
(ID "sur_paren")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(NEWLINE)
(DEDENT)
(ID "hp")
(PUNCT ".")
(ID "set_index")
(PUNCT "(")
(ID "sur_paren")
(PUNCT "[")
(LIT 0)
(PUNCT "]")
(PUNCT ")")
(NEWLINE)
(ID "expression")
(PUNCT "=")
(ID "hp")
(PUNCT ".")
(ID "get_expression")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(KEYWORD not)
(ID "expression")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(NEWLINE)
(DEDENT)
(KEYWORD if)
(KEYWORD not)
(ID "evalfuncs")
(KEYWORD and)
(PUNCT "(")
(ID "expression")
(PUNCT ".")
(ID "find")
(PUNCT "(")
(LIT "(")
(PUNCT ")")
(PUNCT "!=")
(PUNCT "-")
(LIT 1)
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(NEWLINE)
(DEDENT)
(ID "argspec")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "fetch_tip")
(PUNCT "(")
(ID "expression")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(KEYWORD not)
(ID "argspec")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(NEWLINE)
(DEDENT)
(ID "self")
(PUNCT ".")
(ID "active_calltip")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "_calltip_window")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "active_calltip")
(PUNCT ".")
(ID "showtip")
(PUNCT "(")
(ID "argspec")
(PUNCT ",")
(ID "sur_paren")
(PUNCT "[")
(LIT 0)
(PUNCT "]")
(PUNCT ",")
(ID "sur_paren")
(PUNCT "[")
(LIT 1)
(PUNCT "]")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "fetch_tip")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "expression")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Return the argument list and docstring of a function or class.\n\n        If there is a Python subprocess, get the calltip there.  Otherwise,\n        either this fetch_tip() is running in the subprocess or it was\n        called in an IDLE running without the subprocess.\n\n        The subprocess environment is that of the most recently run script.  If\n        two unrelated modules are being edited some calltips in the current\n        module may be inoperative if the module was not the last to run.\n\n        To find methods, fetch_tip must be fed a fully qualified name.\n\n        ")
(NEWLINE)
(KEYWORD try)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "rpcclt")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "editwin")
(PUNCT ".")
(ID "flist")
(PUNCT ".")
(ID "pyshell")
(PUNCT ".")
(ID "interp")
(PUNCT ".")
(ID "rpcclt")
(NEWLINE)
(DEDENT)
(KEYWORD except)
(ID "AttributeError")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "rpcclt")
(PUNCT "=")
(KEYWORD None)
(NEWLINE)
(DEDENT)
(KEYWORD if)
(ID "rpcclt")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(ID "rpcclt")
(PUNCT ".")
(ID "remotecall")
(PUNCT "(")
(LIT "exec")
(PUNCT ",")
(LIT "get_the_calltip")
(PUNCT ",")
(PUNCT "(")
(ID "expression")
(PUNCT ",")
(PUNCT ")")
(PUNCT ",")
(PUNCT "{")
(PUNCT "}")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(ID "get_argspec")
(PUNCT "(")
(ID "get_entity")
(PUNCT "(")
(ID "expression")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(DEDENT)
(KEYWORD def)
(ID "get_entity")
(PUNCT "(")
(ID "expression")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Return the object corresponding to expression evaluated\n    in a namespace spanning sys.modules and __main.dict__.\n    ")
(NEWLINE)
(KEYWORD if)
(ID "expression")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "namespace")
(PUNCT "=")
(ID "sys")
(PUNCT ".")
(ID "modules")
(PUNCT ".")
(ID "copy")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "namespace")
(PUNCT ".")
(ID "update")
(PUNCT "(")
(ID "__main__")
(PUNCT ".")
(ID "__dict__")
(PUNCT ")")
(NEWLINE)
(KEYWORD try)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(ID "eval")
(PUNCT "(")
(ID "expression")
(PUNCT ",")
(ID "namespace")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD except)
(ID "BaseException")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(KEYWORD None)
(NEWLINE)
(DEDENT)
(DEDENT)
(DEDENT)
(ID "_MAX_COLS")
(PUNCT "=")
(LIT 85)
(NEWLINE)
(ID "_MAX_LINES")
(PUNCT "=")
(LIT 5)
(NEWLINE)
(ID "_INDENT")
(PUNCT "=")
(LIT " ")
(PUNCT "*")
(LIT 4)
(NEWLINE)
(ID "_first_param")
(PUNCT "=")
(ID "re")
(PUNCT ".")
(ID "compile")
(PUNCT "(")
(LIT "(?<=\\()\\w*\\,?\\s*")
(PUNCT ")")
(NEWLINE)
(ID "_default_callable_argspec")
(PUNCT "=")
(LIT "See source or doc")
(NEWLINE)
(KEYWORD def)
(ID "get_argspec")
(PUNCT "(")
(ID "ob")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Return a string describing the signature of a callable object, or ''.\n\n    For Python-coded functions and methods, the first line is introspected.\n    Delete 'self' parameter for classes (.__init__) and bound methods.\n    The next lines are the first lines of the doc string up to the first\n    empty line or _MAX_LINES.    For builtins, this typically includes\n    the arguments in addition to the return value.\n    ")
(NEWLINE)
(ID "argspec")
(PUNCT "=")
(LIT "")
(NEWLINE)
(KEYWORD try)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "ob_call")
(PUNCT "=")
(ID "ob")
(PUNCT ".")
(ID "__call__")
(NEWLINE)
(DEDENT)
(KEYWORD except)
(ID "BaseException")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(ID "argspec")
(NEWLINE)
(DEDENT)
(KEYWORD if)
(ID "isinstance")
(PUNCT "(")
(ID "ob")
(PUNCT ",")
(ID "type")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "fob")
(PUNCT "=")
(ID "ob")
(PUNCT ".")
(ID "__init__")
(NEWLINE)
(DEDENT)
(KEYWORD elif)
(ID "isinstance")
(PUNCT "(")
(ID "ob_call")
(PUNCT ",")
(ID "types")
(PUNCT ".")
(ID "MethodType")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "fob")
(PUNCT "=")
(ID "ob_call")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "fob")
(PUNCT "=")
(ID "ob")
(NEWLINE)
(DEDENT)
(KEYWORD if)
(ID "isinstance")
(PUNCT "(")
(ID "fob")
(PUNCT ",")
(PUNCT "(")
(ID "types")
(PUNCT ".")
(ID "FunctionType")
(PUNCT ",")
(ID "types")
(PUNCT ".")
(ID "MethodType")
(PUNCT ")")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "argspec")
(PUNCT "=")
(ID "inspect")
(PUNCT ".")
(ID "formatargspec")
(PUNCT "(")
(PUNCT "*")
(ID "inspect")
(PUNCT ".")
(ID "getfullargspec")
(PUNCT "(")
(ID "fob")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(PUNCT "(")
(ID "isinstance")
(PUNCT "(")
(ID "ob")
(PUNCT ",")
(PUNCT "(")
(ID "type")
(PUNCT ",")
(ID "types")
(PUNCT ".")
(ID "MethodType")
(PUNCT ")")
(PUNCT ")")
(KEYWORD or)
(ID "isinstance")
(PUNCT "(")
(ID "ob_call")
(PUNCT ",")
(ID "types")
(PUNCT ".")
(ID "MethodType")
(PUNCT ")")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "argspec")
(PUNCT "=")
(ID "_first_param")
(PUNCT ".")
(ID "sub")
(PUNCT "(")
(LIT "")
(PUNCT ",")
(ID "argspec")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(ID "lines")
(PUNCT "=")
(PUNCT "(")
(ID "textwrap")
(PUNCT ".")
(ID "wrap")
(PUNCT "(")
(ID "argspec")
(PUNCT ",")
(ID "_MAX_COLS")
(PUNCT ",")
(ID "subsequent_indent")
(PUNCT "=")
(ID "_INDENT")
(PUNCT ")")
(KEYWORD if)
(ID "len")
(PUNCT "(")
(ID "argspec")
(PUNCT ")")
(PUNCT ">")
(ID "_MAX_COLS")
(KEYWORD else)
(PUNCT "[")
(ID "argspec")
(PUNCT "]")
(KEYWORD if)
(ID "argspec")
(KEYWORD else)
(PUNCT "[")
(PUNCT "]")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(ID "isinstance")
(PUNCT "(")
(ID "ob_call")
(PUNCT ",")
(ID "types")
(PUNCT ".")
(ID "MethodType")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "doc")
(PUNCT "=")
(ID "ob_call")
(PUNCT ".")
(ID "__doc__")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "doc")
(PUNCT "=")
(ID "getattr")
(PUNCT "(")
(ID "ob")
(PUNCT ",")
(LIT "__doc__")
(PUNCT ",")
(LIT "")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD if)
(ID "doc")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD for)
(ID "line")
(KEYWORD in)
(ID "doc")
(PUNCT ".")
(ID "split")
(PUNCT "(")
(LIT "\n")
(PUNCT ",")
(ID "_MAX_LINES")
(PUNCT ")")
(PUNCT "[")
(PUNCT ":")
(ID "_MAX_LINES")
(PUNCT "]")
(PUNCT ":")
(NEWLINE)
(INDENT)
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
(KEYWORD break)
(NEWLINE)
(DEDENT)
(KEYWORD if)
(ID "len")
(PUNCT "(")
(ID "line")
(PUNCT ")")
(PUNCT ">")
(ID "_MAX_COLS")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "line")
(PUNCT "=")
(ID "line")
(PUNCT "[")
(PUNCT ":")
(ID "_MAX_COLS")
(PUNCT "-")
(LIT 3)
(PUNCT "]")
(PUNCT "+")
(LIT "...")
(NEWLINE)
(DEDENT)
(ID "lines")
(PUNCT ".")
(ID "append")
(PUNCT "(")
(ID "line")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "argspec")
(PUNCT "=")
(LIT "\n")
(PUNCT ".")
(ID "join")
(PUNCT "(")
(ID "lines")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD if)
(KEYWORD not)
(ID "argspec")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "argspec")
(PUNCT "=")
(ID "_default_callable_argspec")
(NEWLINE)
(DEDENT)
(KEYWORD return)
(ID "argspec")
(NEWLINE)
(DEDENT)
(KEYWORD if)
(ID "__name__")
(PUNCT "==")
(LIT "__main__")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD from)
(ID "unittest")
(KEYWORD import)
(ID "main")
(NEWLINE)
(ID "main")
(PUNCT "(")
(LIT "idlelib.idle_test.test_calltips")
(PUNCT ",")
(ID "verbosity")
(PUNCT "=")
(LIT 2)
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ENDMARKER)
