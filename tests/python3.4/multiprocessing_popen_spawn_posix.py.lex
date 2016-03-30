(KEYWORD import)
(ID "io")
(NEWLINE)
(KEYWORD import)
(ID "os")
(NEWLINE)
(KEYWORD from)
(PUNCT ".")
(KEYWORD import)
(ID "context")
(NEWLINE)
(KEYWORD from)
(PUNCT ".")
(KEYWORD import)
(ID "popen_fork")
(NEWLINE)
(KEYWORD from)
(PUNCT ".")
(KEYWORD import)
(ID "reduction")
(NEWLINE)
(KEYWORD from)
(PUNCT ".")
(KEYWORD import)
(ID "spawn")
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
(ID "_DupFd")
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
(ID "fd")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "fd")
(PUNCT "=")
(ID "fd")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "detach")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(ID "self")
(PUNCT ".")
(ID "fd")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD class)
(ID "Popen")
(PUNCT "(")
(ID "popen_fork")
(PUNCT ".")
(ID "Popen")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "method")
(PUNCT "=")
(LIT "spawn")
(NEWLINE)
(ID "DupFd")
(PUNCT "=")
(ID "_DupFd")
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
(ID "self")
(PUNCT ".")
(ID "_fds")
(PUNCT "=")
(PUNCT "[")
(PUNCT "]")
(NEWLINE)
(ID "super")
(PUNCT "(")
(PUNCT ")")
(PUNCT ".")
(ID "__init__")
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
(ID "self")
(PUNCT ".")
(ID "_fds")
(PUNCT ".")
(ID "append")
(PUNCT "(")
(ID "fd")
(PUNCT ")")
(NEWLINE)
(KEYWORD return)
(ID "fd")
(NEWLINE)
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
(KEYWORD from)
(PUNCT ".")
(KEYWORD import)
(ID "semaphore_tracker")
(NEWLINE)
(ID "tracker_fd")
(PUNCT "=")
(ID "semaphore_tracker")
(PUNCT ".")
(ID "getfd")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_fds")
(PUNCT ".")
(ID "append")
(PUNCT "(")
(ID "tracker_fd")
(PUNCT ")")
(NEWLINE)
(ID "prep_data")
(PUNCT "=")
(ID "spawn")
(PUNCT ".")
(ID "get_preparation_data")
(PUNCT "(")
(ID "process_obj")
(PUNCT ".")
(ID "_name")
(PUNCT ")")
(NEWLINE)
(ID "fp")
(PUNCT "=")
(ID "io")
(PUNCT ".")
(ID "BytesIO")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "context")
(PUNCT ".")
(ID "set_spawning_popen")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(NEWLINE)
(KEYWORD try)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "reduction")
(PUNCT ".")
(ID "dump")
(PUNCT "(")
(ID "prep_data")
(PUNCT ",")
(ID "fp")
(PUNCT ")")
(NEWLINE)
(ID "reduction")
(PUNCT ".")
(ID "dump")
(PUNCT "(")
(ID "process_obj")
(PUNCT ",")
(ID "fp")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD finally)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "context")
(PUNCT ".")
(ID "set_spawning_popen")
(PUNCT "(")
(KEYWORD None)
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "parent_r")
(PUNCT "=")
(ID "child_w")
(PUNCT "=")
(ID "child_r")
(PUNCT "=")
(ID "parent_w")
(PUNCT "=")
(KEYWORD None)
(NEWLINE)
(KEYWORD try)
(PUNCT ":")
(NEWLINE)
(INDENT)
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
(ID "child_r")
(PUNCT ",")
(ID "parent_w")
(PUNCT "=")
(ID "os")
(PUNCT ".")
(ID "pipe")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "cmd")
(PUNCT "=")
(ID "spawn")
(PUNCT ".")
(ID "get_command_line")
(PUNCT "(")
(ID "tracker_fd")
(PUNCT "=")
(ID "tracker_fd")
(PUNCT ",")
(ID "pipe_handle")
(PUNCT "=")
(ID "child_r")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_fds")
(PUNCT ".")
(ID "extend")
(PUNCT "(")
(PUNCT "[")
(ID "child_r")
(PUNCT ",")
(ID "child_w")
(PUNCT "]")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "pid")
(PUNCT "=")
(ID "util")
(PUNCT ".")
(ID "spawnv_passfds")
(PUNCT "(")
(ID "spawn")
(PUNCT ".")
(ID "get_executable")
(PUNCT "(")
(PUNCT ")")
(PUNCT ",")
(ID "cmd")
(PUNCT ",")
(ID "self")
(PUNCT ".")
(ID "_fds")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "sentinel")
(PUNCT "=")
(ID "parent_r")
(NEWLINE)
(KEYWORD with)
(ID "open")
(PUNCT "(")
(ID "parent_w")
(PUNCT ",")
(LIT "wb")
(PUNCT ",")
(ID "closefd")
(PUNCT "=")
(KEYWORD False)
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
(ID "fp")
(PUNCT ".")
(ID "getbuffer")
(PUNCT "(")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD finally)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "parent_r")
(KEYWORD is)
(KEYWORD not)
(KEYWORD None)
(PUNCT ":")
(NEWLINE)
(INDENT)
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
(DEDENT)
(KEYWORD for)
(ID "fd")
(KEYWORD in)
(PUNCT "(")
(ID "child_r")
(PUNCT ",")
(ID "child_w")
(PUNCT ",")
(ID "parent_w")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "fd")
(KEYWORD is)
(KEYWORD not)
(KEYWORD None)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "os")
(PUNCT ".")
(ID "close")
(PUNCT "(")
(ID "fd")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(DEDENT)
(DEDENT)
(DEDENT)
(ENDMARKER)
