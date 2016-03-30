(KEYWORD import)
(ID "os")
(NEWLINE)
(KEYWORD import)
(ID "sys")
(NEWLINE)
(KEYWORD import)
(ID "signal")
(NEWLINE)
(KEYWORD import)
(ID "errno")
(NEWLINE)
(KEYWORD from)
(PUNCT ".")
(KEYWORD import)
(ID "util")
(NEWLINE)
(ID "__all__")
(PUNCT "=")
(PUNCT "[")
(LIT "Popen")
(PUNCT "]")
(NEWLINE)
(KEYWORD class)
(ID "Popen")
(PUNCT "(")
(ID "object")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "method")
(PUNCT "=")
(LIT "fork")
(NEWLINE)
(KEYWORD def)
(ID "__init__")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "process_obj")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "sys")
(PUNCT ".")
(ID "stdout")
(PUNCT ".")
(ID "flush")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "sys")
(PUNCT ".")
(ID "stderr")
(PUNCT ".")
(ID "flush")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "returncode")
(PUNCT "=")
(KEYWORD None)
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_launch")
(PUNCT "(")
(ID "process_obj")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "duplicate_for_child")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "fd")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(ID "fd")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "poll")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "flag")
(PUNCT "=")
(ID "os")
(PUNCT ".")
(ID "WNOHANG")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "self")
(PUNCT ".")
(ID "returncode")
(KEYWORD is)
(KEYWORD None)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD while)
(KEYWORD True)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD try)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "pid")
(PUNCT ",")
(ID "sts")
(PUNCT "=")
(ID "os")
(PUNCT ".")
(ID "waitpid")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "pid")
(PUNCT ",")
(ID "flag")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD except)
(ID "OSError")
(KEYWORD as)
(ID "e")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "e")
(PUNCT ".")
(ID "errno")
(PUNCT "==")
(ID "errno")
(PUNCT ".")
(ID "EINTR")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD continue)
(NEWLINE)
(DEDENT)
(KEYWORD return)
(KEYWORD None)
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD break)
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD if)
(ID "pid")
(PUNCT "==")
(ID "self")
(PUNCT ".")
(ID "pid")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "os")
(PUNCT ".")
(ID "WIFSIGNALED")
(PUNCT "(")
(ID "sts")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "returncode")
(PUNCT "=")
(PUNCT "-")
(ID "os")
(PUNCT ".")
(ID "WTERMSIG")
(PUNCT "(")
(ID "sts")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD assert)
(ID "os")
(PUNCT ".")
(ID "WIFEXITED")
(PUNCT "(")
(ID "sts")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "returncode")
(PUNCT "=")
(ID "os")
(PUNCT ".")
(ID "WEXITSTATUS")
(PUNCT "(")
(ID "sts")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(DEDENT)
(KEYWORD return)
(ID "self")
(PUNCT ".")
(ID "returncode")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "wait")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "timeout")
(PUNCT "=")
(KEYWORD None)
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "self")
(PUNCT ".")
(ID "returncode")
(KEYWORD is)
(KEYWORD None)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "timeout")
(KEYWORD is)
(KEYWORD not)
(KEYWORD None)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD from)
(ID "multiprocessing")
(PUNCT ".")
(ID "connection")
(KEYWORD import)
(ID "wait")
(NEWLINE)
(KEYWORD if)
(KEYWORD not)
(ID "wait")
(PUNCT "(")
(PUNCT "[")
(ID "self")
(PUNCT ".")
(ID "sentinel")
(PUNCT "]")
(PUNCT ",")
(ID "timeout")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(KEYWORD None)
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD return)
(ID "self")
(PUNCT ".")
(ID "poll")
(PUNCT "(")
(ID "os")
(PUNCT ".")
(ID "WNOHANG")
(KEYWORD if)
(ID "timeout")
(PUNCT "==")
(LIT 0.0)
(KEYWORD else)
(LIT 0)
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD return)
(ID "self")
(PUNCT ".")
(ID "returncode")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "terminate")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "self")
(PUNCT ".")
(ID "returncode")
(KEYWORD is)
(KEYWORD None)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD try)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "os")
(PUNCT ".")
(ID "kill")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "pid")
(PUNCT ",")
(ID "signal")
(PUNCT ".")
(ID "SIGTERM")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD except)
(ID "ProcessLookupError")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD pass)
(NEWLINE)
(DEDENT)
(KEYWORD except)
(ID "OSError")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "self")
(PUNCT ".")
(ID "wait")
(PUNCT "(")
(ID "timeout")
(PUNCT "=")
(LIT 0.1)
(PUNCT ")")
(KEYWORD is)
(KEYWORD None)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD raise)
(NEWLINE)
(DEDENT)
(DEDENT)
(DEDENT)
(DEDENT)
(KEYWORD def)
(ID "_launch")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "process_obj")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "code")
(PUNCT "=")
(LIT 1)
(NEWLINE)
(ID "parent_r")
(PUNCT ",")
(ID "child_w")
(PUNCT "=")
(ID "os")
(PUNCT ".")
(ID "pipe")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "pid")
(PUNCT "=")
(ID "os")
(PUNCT ".")
(ID "fork")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(ID "self")
(PUNCT ".")
(ID "pid")
(PUNCT "==")
(LIT 0)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD try)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "os")
(PUNCT ".")
(ID "close")
(PUNCT "(")
(ID "parent_r")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(LIT "random")
(KEYWORD in)
(ID "sys")
(PUNCT ".")
(ID "modules")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD import)
(ID "random")
(NEWLINE)
(ID "random")
(PUNCT ".")
(ID "seed")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "code")
(PUNCT "=")
(ID "process_obj")
(PUNCT ".")
(ID "_bootstrap")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD finally)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "os")
(PUNCT ".")
(ID "_exit")
(PUNCT "(")
(ID "code")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "os")
(PUNCT ".")
(ID "close")
(PUNCT "(")
(ID "child_w")
(PUNCT ")")
(NEWLINE)
(ID "util")
(PUNCT ".")
(ID "Finalize")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "os")
(PUNCT ".")
(ID "close")
(PUNCT ",")
(PUNCT "(")
(ID "parent_r")
(PUNCT ",")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "sentinel")
(PUNCT "=")
(ID "parent_r")
(NEWLINE)
(DEDENT)
(DEDENT)
(DEDENT)
(ENDMARKER)
