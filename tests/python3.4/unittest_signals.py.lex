(KEYWORD import)
(ID "signal")
(NEWLINE)
(KEYWORD import)
(ID "weakref")
(NEWLINE)
(KEYWORD from)
(ID "functools")
(KEYWORD import)
(ID "wraps")
(NEWLINE)
(ID "__unittest")
(PUNCT "=")
(KEYWORD True)
(NEWLINE)
(KEYWORD class)
(ID "_InterruptHandler")
(PUNCT "(")
(ID "object")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD def)
(ID "__init__")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "default_handler")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "called")
(PUNCT "=")
(KEYWORD False)
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "original_handler")
(PUNCT "=")
(ID "default_handler")
(NEWLINE)
(KEYWORD if)
(ID "isinstance")
(PUNCT "(")
(ID "default_handler")
(PUNCT ",")
(ID "int")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "default_handler")
(PUNCT "==")
(ID "signal")
(PUNCT ".")
(ID "SIG_DFL")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "default_handler")
(PUNCT "=")
(ID "signal")
(PUNCT ".")
(ID "default_int_handler")
(NEWLINE)
(DEDENT)
(KEYWORD elif)
(ID "default_handler")
(PUNCT "==")
(ID "signal")
(PUNCT ".")
(ID "SIG_IGN")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD def)
(ID "default_handler")
(PUNCT "(")
(ID "unused_signum")
(PUNCT ",")
(ID "unused_frame")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD pass)
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD raise)
(ID "TypeError")
(PUNCT "(")
(LIT "expected SIGINT signal handler to be ")
(LIT "signal.SIG_IGN, signal.SIG_DFL, or a ")
(LIT "callable object")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(ID "self")
(PUNCT ".")
(ID "default_handler")
(PUNCT "=")
(ID "default_handler")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "__call__")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "signum")
(PUNCT ",")
(ID "frame")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "installed_handler")
(PUNCT "=")
(ID "signal")
(PUNCT ".")
(ID "getsignal")
(PUNCT "(")
(ID "signal")
(PUNCT ".")
(ID "SIGINT")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(ID "installed_handler")
(KEYWORD is)
(KEYWORD not)
(ID "self")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "default_handler")
(PUNCT "(")
(ID "signum")
(PUNCT ",")
(ID "frame")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD if)
(ID "self")
(PUNCT ".")
(ID "called")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "default_handler")
(PUNCT "(")
(ID "signum")
(PUNCT ",")
(ID "frame")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "self")
(PUNCT ".")
(ID "called")
(PUNCT "=")
(KEYWORD True)
(NEWLINE)
(KEYWORD for)
(ID "result")
(KEYWORD in)
(ID "_results")
(PUNCT ".")
(ID "keys")
(PUNCT "(")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "result")
(PUNCT ".")
(ID "stop")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(DEDENT)
(ID "_results")
(PUNCT "=")
(ID "weakref")
(PUNCT ".")
(ID "WeakKeyDictionary")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(KEYWORD def)
(ID "registerResult")
(PUNCT "(")
(ID "result")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "_results")
(PUNCT "[")
(ID "result")
(PUNCT "]")
(PUNCT "=")
(LIT 1)
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "removeResult")
(PUNCT "(")
(ID "result")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(ID "bool")
(PUNCT "(")
(ID "_results")
(PUNCT ".")
(ID "pop")
(PUNCT "(")
(ID "result")
(PUNCT ",")
(KEYWORD None)
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "_interrupt_handler")
(PUNCT "=")
(KEYWORD None)
(NEWLINE)
(KEYWORD def)
(ID "installHandler")
(PUNCT "(")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD global)
(ID "_interrupt_handler")
(NEWLINE)
(KEYWORD if)
(ID "_interrupt_handler")
(KEYWORD is)
(KEYWORD None)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "default_handler")
(PUNCT "=")
(ID "signal")
(PUNCT ".")
(ID "getsignal")
(PUNCT "(")
(ID "signal")
(PUNCT ".")
(ID "SIGINT")
(PUNCT ")")
(NEWLINE)
(ID "_interrupt_handler")
(PUNCT "=")
(ID "_InterruptHandler")
(PUNCT "(")
(ID "default_handler")
(PUNCT ")")
(NEWLINE)
(ID "signal")
(PUNCT ".")
(ID "signal")
(PUNCT "(")
(ID "signal")
(PUNCT ".")
(ID "SIGINT")
(PUNCT ",")
(ID "_interrupt_handler")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD def)
(ID "removeHandler")
(PUNCT "(")
(ID "method")
(PUNCT "=")
(KEYWORD None)
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "method")
(KEYWORD is)
(KEYWORD not)
(KEYWORD None)
(PUNCT ":")
(NEWLINE)
(INDENT)
(PUNCT "@")
(ID "wraps")
(PUNCT "(")
(ID "method")
(PUNCT ")")
(NEWLINE)
(KEYWORD def)
(ID "inner")
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
(ID "initial")
(PUNCT "=")
(ID "signal")
(PUNCT ".")
(ID "getsignal")
(PUNCT "(")
(ID "signal")
(PUNCT ".")
(ID "SIGINT")
(PUNCT ")")
(NEWLINE)
(ID "removeHandler")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(KEYWORD try)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(ID "method")
(PUNCT "(")
(PUNCT "*")
(ID "args")
(PUNCT ",")
(PUNCT "**")
(ID "kwargs")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD finally)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "signal")
(PUNCT ".")
(ID "signal")
(PUNCT "(")
(ID "signal")
(PUNCT ".")
(ID "SIGINT")
(PUNCT ",")
(ID "initial")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD return)
(ID "inner")
(NEWLINE)
(DEDENT)
(KEYWORD global)
(ID "_interrupt_handler")
(NEWLINE)
(KEYWORD if)
(ID "_interrupt_handler")
(KEYWORD is)
(KEYWORD not)
(KEYWORD None)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "signal")
(PUNCT ".")
(ID "signal")
(PUNCT "(")
(ID "signal")
(PUNCT ".")
(ID "SIGINT")
(PUNCT ",")
(ID "_interrupt_handler")
(PUNCT ".")
(ID "original_handler")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(ENDMARKER)
