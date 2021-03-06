(ID "__all__")
(PUNCT "=")
(PUNCT "[")
(LIT "create_subprocess_exec")
(PUNCT ",")
(LIT "create_subprocess_shell")
(PUNCT "]")
(NEWLINE)
(KEYWORD import)
(ID "collections")
(NEWLINE)
(KEYWORD import)
(ID "subprocess")
(NEWLINE)
(KEYWORD from)
(PUNCT ".")
(KEYWORD import)
(ID "events")
(NEWLINE)
(KEYWORD from)
(PUNCT ".")
(KEYWORD import)
(ID "futures")
(NEWLINE)
(KEYWORD from)
(PUNCT ".")
(KEYWORD import)
(ID "protocols")
(NEWLINE)
(KEYWORD from)
(PUNCT ".")
(KEYWORD import)
(ID "streams")
(NEWLINE)
(KEYWORD from)
(PUNCT ".")
(KEYWORD import)
(ID "tasks")
(NEWLINE)
(KEYWORD from)
(PUNCT ".")
(ID "coroutines")
(KEYWORD import)
(ID "coroutine")
(NEWLINE)
(KEYWORD from)
(PUNCT ".")
(ID "log")
(KEYWORD import)
(ID "logger")
(NEWLINE)
(ID "PIPE")
(PUNCT "=")
(ID "subprocess")
(PUNCT ".")
(ID "PIPE")
(NEWLINE)
(ID "STDOUT")
(PUNCT "=")
(ID "subprocess")
(PUNCT ".")
(ID "STDOUT")
(NEWLINE)
(ID "DEVNULL")
(PUNCT "=")
(ID "subprocess")
(PUNCT ".")
(ID "DEVNULL")
(NEWLINE)
(KEYWORD class)
(ID "SubprocessStreamProtocol")
(PUNCT "(")
(ID "streams")
(PUNCT ".")
(ID "FlowControlMixin")
(PUNCT ",")
(ID "protocols")
(PUNCT ".")
(ID "SubprocessProtocol")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Like StreamReaderProtocol, but for a subprocess.")
(NEWLINE)
(KEYWORD def)
(ID "__init__")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "limit")
(PUNCT ",")
(ID "loop")
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
(ID "loop")
(PUNCT "=")
(ID "loop")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_limit")
(PUNCT "=")
(ID "limit")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "stdin")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "stdout")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "stderr")
(PUNCT "=")
(KEYWORD None)
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_transport")
(PUNCT "=")
(KEYWORD None)
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "__repr__")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "info")
(PUNCT "=")
(PUNCT "[")
(ID "self")
(PUNCT ".")
(ID "__class__")
(PUNCT ".")
(ID "__name__")
(PUNCT "]")
(NEWLINE)
(KEYWORD if)
(ID "self")
(PUNCT ".")
(ID "stdin")
(KEYWORD is)
(KEYWORD not)
(KEYWORD None)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "info")
(PUNCT ".")
(ID "append")
(PUNCT "(")
(LIT "stdin=%r")
(PUNCT "%")
(ID "self")
(PUNCT ".")
(ID "stdin")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD if)
(ID "self")
(PUNCT ".")
(ID "stdout")
(KEYWORD is)
(KEYWORD not)
(KEYWORD None)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "info")
(PUNCT ".")
(ID "append")
(PUNCT "(")
(LIT "stdout=%r")
(PUNCT "%")
(ID "self")
(PUNCT ".")
(ID "stdout")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD if)
(ID "self")
(PUNCT ".")
(ID "stderr")
(KEYWORD is)
(KEYWORD not)
(KEYWORD None)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "info")
(PUNCT ".")
(ID "append")
(PUNCT "(")
(LIT "stderr=%r")
(PUNCT "%")
(ID "self")
(PUNCT ".")
(ID "stderr")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD return)
(LIT "<%s>")
(PUNCT "%")
(LIT " ")
(PUNCT ".")
(ID "join")
(PUNCT "(")
(ID "info")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "connection_made")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "transport")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "_transport")
(PUNCT "=")
(ID "transport")
(NEWLINE)
(ID "stdout_transport")
(PUNCT "=")
(ID "transport")
(PUNCT ".")
(ID "get_pipe_transport")
(PUNCT "(")
(LIT 1)
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(ID "stdout_transport")
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
(ID "streams")
(PUNCT ".")
(ID "StreamReader")
(PUNCT "(")
(ID "limit")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "_limit")
(PUNCT ",")
(ID "loop")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "_loop")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "stdout")
(PUNCT ".")
(ID "set_transport")
(PUNCT "(")
(ID "stdout_transport")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "stderr_transport")
(PUNCT "=")
(ID "transport")
(PUNCT ".")
(ID "get_pipe_transport")
(PUNCT "(")
(LIT 2)
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(ID "stderr_transport")
(KEYWORD is)
(KEYWORD not)
(KEYWORD None)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "stderr")
(PUNCT "=")
(ID "streams")
(PUNCT ".")
(ID "StreamReader")
(PUNCT "(")
(ID "limit")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "_limit")
(PUNCT ",")
(ID "loop")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "_loop")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "stderr")
(PUNCT ".")
(ID "set_transport")
(PUNCT "(")
(ID "stderr_transport")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "stdin_transport")
(PUNCT "=")
(ID "transport")
(PUNCT ".")
(ID "get_pipe_transport")
(PUNCT "(")
(LIT 0)
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(ID "stdin_transport")
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
(ID "streams")
(PUNCT ".")
(ID "StreamWriter")
(PUNCT "(")
(ID "stdin_transport")
(PUNCT ",")
(ID "protocol")
(PUNCT "=")
(ID "self")
(PUNCT ",")
(ID "reader")
(PUNCT "=")
(KEYWORD None)
(PUNCT ",")
(ID "loop")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "_loop")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD def)
(ID "pipe_data_received")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "fd")
(PUNCT ",")
(ID "data")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "fd")
(PUNCT "==")
(LIT 1)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "reader")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "stdout")
(NEWLINE)
(DEDENT)
(KEYWORD elif)
(ID "fd")
(PUNCT "==")
(LIT 2)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "reader")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "stderr")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "reader")
(PUNCT "=")
(KEYWORD None)
(NEWLINE)
(DEDENT)
(KEYWORD if)
(ID "reader")
(KEYWORD is)
(KEYWORD not)
(KEYWORD None)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "reader")
(PUNCT ".")
(ID "feed_data")
(PUNCT "(")
(ID "data")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD def)
(ID "pipe_connection_lost")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "fd")
(PUNCT ",")
(ID "exc")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "fd")
(PUNCT "==")
(LIT 0)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "pipe")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "stdin")
(NEWLINE)
(KEYWORD if)
(ID "pipe")
(KEYWORD is)
(KEYWORD not)
(KEYWORD None)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "pipe")
(PUNCT ".")
(ID "close")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "self")
(PUNCT ".")
(ID "connection_lost")
(PUNCT "(")
(ID "exc")
(PUNCT ")")
(NEWLINE)
(KEYWORD return)
(NEWLINE)
(DEDENT)
(KEYWORD if)
(ID "fd")
(PUNCT "==")
(LIT 1)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "reader")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "stdout")
(NEWLINE)
(DEDENT)
(KEYWORD elif)
(ID "fd")
(PUNCT "==")
(LIT 2)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "reader")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "stderr")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "reader")
(PUNCT "=")
(KEYWORD None)
(NEWLINE)
(DEDENT)
(KEYWORD if)
(ID "reader")
(PUNCT "!=")
(KEYWORD None)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "exc")
(KEYWORD is)
(KEYWORD None)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "reader")
(PUNCT ".")
(ID "feed_eof")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "reader")
(PUNCT ".")
(ID "set_exception")
(PUNCT "(")
(ID "exc")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(DEDENT)
(KEYWORD def)
(ID "process_exited")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "_transport")
(PUNCT ".")
(ID "close")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_transport")
(PUNCT "=")
(KEYWORD None)
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD class)
(ID "Process")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD def)
(ID "__init__")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "transport")
(PUNCT ",")
(ID "protocol")
(PUNCT ",")
(ID "loop")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "_transport")
(PUNCT "=")
(ID "transport")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_protocol")
(PUNCT "=")
(ID "protocol")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_loop")
(PUNCT "=")
(ID "loop")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "stdin")
(PUNCT "=")
(ID "protocol")
(PUNCT ".")
(ID "stdin")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "stdout")
(PUNCT "=")
(ID "protocol")
(PUNCT ".")
(ID "stdout")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "stderr")
(PUNCT "=")
(ID "protocol")
(PUNCT ".")
(ID "stderr")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "pid")
(PUNCT "=")
(ID "transport")
(PUNCT ".")
(ID "get_pid")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "__repr__")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(LIT "<%s %s>")
(PUNCT "%")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "__class__")
(PUNCT ".")
(ID "__name__")
(PUNCT ",")
(ID "self")
(PUNCT ".")
(ID "pid")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(PUNCT "@")
(ID "property")
(NEWLINE)
(KEYWORD def)
(ID "returncode")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(ID "self")
(PUNCT ".")
(ID "_transport")
(PUNCT ".")
(ID "get_returncode")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(PUNCT "@")
(ID "coroutine")
(NEWLINE)
(KEYWORD def)
(ID "wait")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Wait until the process exit and return the process return code.\n\n        This method is a coroutine.")
(NEWLINE)
(KEYWORD return)
(PUNCT "(")
(KEYWORD yield)
(KEYWORD from)
(ID "self")
(PUNCT ".")
(ID "_transport")
(PUNCT ".")
(ID "_wait")
(PUNCT "(")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "send_signal")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "signal")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "_transport")
(PUNCT ".")
(ID "send_signal")
(PUNCT "(")
(ID "signal")
(PUNCT ")")
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
(ID "self")
(PUNCT ".")
(ID "_transport")
(PUNCT ".")
(ID "terminate")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "kill")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "_transport")
(PUNCT ".")
(ID "kill")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(PUNCT "@")
(ID "coroutine")
(NEWLINE)
(KEYWORD def)
(ID "_feed_stdin")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "input")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "debug")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "_loop")
(PUNCT ".")
(ID "get_debug")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "stdin")
(PUNCT ".")
(ID "write")
(PUNCT "(")
(ID "input")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(ID "debug")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "logger")
(PUNCT ".")
(ID "debug")
(PUNCT "(")
(LIT "%r communicate: feed stdin (%s bytes)")
(PUNCT ",")
(ID "self")
(PUNCT ",")
(ID "len")
(PUNCT "(")
(ID "input")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD try)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD yield)
(KEYWORD from)
(ID "self")
(PUNCT ".")
(ID "stdin")
(PUNCT ".")
(ID "drain")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD except)
(PUNCT "(")
(ID "BrokenPipeError")
(PUNCT ",")
(ID "ConnectionResetError")
(PUNCT ")")
(KEYWORD as)
(ID "exc")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "debug")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "logger")
(PUNCT ".")
(ID "debug")
(PUNCT "(")
(LIT "%r communicate: stdin got %r")
(PUNCT ",")
(ID "self")
(PUNCT ",")
(ID "exc")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD if)
(ID "debug")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "logger")
(PUNCT ".")
(ID "debug")
(PUNCT "(")
(LIT "%r communicate: close stdin")
(PUNCT ",")
(ID "self")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "self")
(PUNCT ".")
(ID "stdin")
(PUNCT ".")
(ID "close")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(PUNCT "@")
(ID "coroutine")
(NEWLINE)
(KEYWORD def)
(ID "_noop")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(KEYWORD None)
(NEWLINE)
(DEDENT)
(PUNCT "@")
(ID "coroutine")
(NEWLINE)
(KEYWORD def)
(ID "_read_stream")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "fd")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "transport")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "_transport")
(PUNCT ".")
(ID "get_pipe_transport")
(PUNCT "(")
(ID "fd")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(ID "fd")
(PUNCT "==")
(LIT 2)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "stream")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "stderr")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD assert)
(ID "fd")
(PUNCT "==")
(LIT 1)
(NEWLINE)
(ID "stream")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "stdout")
(NEWLINE)
(DEDENT)
(KEYWORD if)
(ID "self")
(PUNCT ".")
(ID "_loop")
(PUNCT ".")
(ID "get_debug")
(PUNCT "(")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "name")
(PUNCT "=")
(LIT "stdout")
(KEYWORD if)
(ID "fd")
(PUNCT "==")
(LIT 1)
(KEYWORD else)
(LIT "stderr")
(NEWLINE)
(ID "logger")
(PUNCT ".")
(ID "debug")
(PUNCT "(")
(LIT "%r communicate: read %s")
(PUNCT ",")
(ID "self")
(PUNCT ",")
(ID "name")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "output")
(PUNCT "=")
(KEYWORD yield)
(KEYWORD from)
(ID "stream")
(PUNCT ".")
(ID "read")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(ID "self")
(PUNCT ".")
(ID "_loop")
(PUNCT ".")
(ID "get_debug")
(PUNCT "(")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "name")
(PUNCT "=")
(LIT "stdout")
(KEYWORD if)
(ID "fd")
(PUNCT "==")
(LIT 1)
(KEYWORD else)
(LIT "stderr")
(NEWLINE)
(ID "logger")
(PUNCT ".")
(ID "debug")
(PUNCT "(")
(LIT "%r communicate: close %s")
(PUNCT ",")
(ID "self")
(PUNCT ",")
(ID "name")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "transport")
(PUNCT ".")
(ID "close")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(KEYWORD return)
(ID "output")
(NEWLINE)
(DEDENT)
(PUNCT "@")
(ID "coroutine")
(NEWLINE)
(KEYWORD def)
(ID "communicate")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "input")
(PUNCT "=")
(KEYWORD None)
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "input")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "stdin")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "_feed_stdin")
(PUNCT "(")
(ID "input")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "stdin")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "_noop")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD if)
(ID "self")
(PUNCT ".")
(ID "stdout")
(KEYWORD is)
(KEYWORD not)
(KEYWORD None)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "stdout")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "_read_stream")
(PUNCT "(")
(LIT 1)
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "stdout")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "_noop")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD if)
(ID "self")
(PUNCT ".")
(ID "stderr")
(KEYWORD is)
(KEYWORD not)
(KEYWORD None)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "stderr")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "_read_stream")
(PUNCT "(")
(LIT 2)
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "stderr")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "_noop")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "stdin")
(PUNCT ",")
(ID "stdout")
(PUNCT ",")
(ID "stderr")
(PUNCT "=")
(KEYWORD yield)
(KEYWORD from)
(ID "tasks")
(PUNCT ".")
(ID "gather")
(PUNCT "(")
(ID "stdin")
(PUNCT ",")
(ID "stdout")
(PUNCT ",")
(ID "stderr")
(PUNCT ",")
(ID "loop")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "_loop")
(PUNCT ")")
(NEWLINE)
(KEYWORD yield)
(KEYWORD from)
(ID "self")
(PUNCT ".")
(ID "wait")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(KEYWORD return)
(PUNCT "(")
(ID "stdout")
(PUNCT ",")
(ID "stderr")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(PUNCT "@")
(ID "coroutine")
(NEWLINE)
(KEYWORD def)
(ID "create_subprocess_shell")
(PUNCT "(")
(ID "cmd")
(PUNCT ",")
(ID "stdin")
(PUNCT "=")
(KEYWORD None)
(PUNCT ",")
(ID "stdout")
(PUNCT "=")
(KEYWORD None)
(PUNCT ",")
(ID "stderr")
(PUNCT "=")
(KEYWORD None)
(PUNCT ",")
(ID "loop")
(PUNCT "=")
(KEYWORD None)
(PUNCT ",")
(ID "limit")
(PUNCT "=")
(ID "streams")
(PUNCT ".")
(ID "_DEFAULT_LIMIT")
(PUNCT ",")
(PUNCT "**")
(ID "kwds")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "loop")
(KEYWORD is)
(KEYWORD None)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "loop")
(PUNCT "=")
(ID "events")
(PUNCT ".")
(ID "get_event_loop")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "protocol_factory")
(PUNCT "=")
(KEYWORD lambda)
(PUNCT ":")
(ID "SubprocessStreamProtocol")
(PUNCT "(")
(ID "limit")
(PUNCT "=")
(ID "limit")
(PUNCT ",")
(ID "loop")
(PUNCT "=")
(ID "loop")
(PUNCT ")")
(NEWLINE)
(ID "transport")
(PUNCT ",")
(ID "protocol")
(PUNCT "=")
(KEYWORD yield)
(KEYWORD from)
(ID "loop")
(PUNCT ".")
(ID "subprocess_shell")
(PUNCT "(")
(ID "protocol_factory")
(PUNCT ",")
(ID "cmd")
(PUNCT ",")
(ID "stdin")
(PUNCT "=")
(ID "stdin")
(PUNCT ",")
(ID "stdout")
(PUNCT "=")
(ID "stdout")
(PUNCT ",")
(ID "stderr")
(PUNCT "=")
(ID "stderr")
(PUNCT ",")
(PUNCT "**")
(ID "kwds")
(PUNCT ")")
(NEWLINE)
(KEYWORD return)
(ID "Process")
(PUNCT "(")
(ID "transport")
(PUNCT ",")
(ID "protocol")
(PUNCT ",")
(ID "loop")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(PUNCT "@")
(ID "coroutine")
(NEWLINE)
(KEYWORD def)
(ID "create_subprocess_exec")
(PUNCT "(")
(ID "program")
(PUNCT ",")
(PUNCT "*")
(ID "args")
(PUNCT ",")
(ID "stdin")
(PUNCT "=")
(KEYWORD None)
(PUNCT ",")
(ID "stdout")
(PUNCT "=")
(KEYWORD None)
(PUNCT ",")
(ID "stderr")
(PUNCT "=")
(KEYWORD None)
(PUNCT ",")
(ID "loop")
(PUNCT "=")
(KEYWORD None)
(PUNCT ",")
(ID "limit")
(PUNCT "=")
(ID "streams")
(PUNCT ".")
(ID "_DEFAULT_LIMIT")
(PUNCT ",")
(PUNCT "**")
(ID "kwds")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "loop")
(KEYWORD is)
(KEYWORD None)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "loop")
(PUNCT "=")
(ID "events")
(PUNCT ".")
(ID "get_event_loop")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "protocol_factory")
(PUNCT "=")
(KEYWORD lambda)
(PUNCT ":")
(ID "SubprocessStreamProtocol")
(PUNCT "(")
(ID "limit")
(PUNCT "=")
(ID "limit")
(PUNCT ",")
(ID "loop")
(PUNCT "=")
(ID "loop")
(PUNCT ")")
(NEWLINE)
(ID "transport")
(PUNCT ",")
(ID "protocol")
(PUNCT "=")
(KEYWORD yield)
(KEYWORD from)
(ID "loop")
(PUNCT ".")
(ID "subprocess_exec")
(PUNCT "(")
(ID "protocol_factory")
(PUNCT ",")
(ID "program")
(PUNCT ",")
(PUNCT "*")
(ID "args")
(PUNCT ",")
(ID "stdin")
(PUNCT "=")
(ID "stdin")
(PUNCT ",")
(ID "stdout")
(PUNCT "=")
(ID "stdout")
(PUNCT ",")
(ID "stderr")
(PUNCT "=")
(ID "stderr")
(PUNCT ",")
(PUNCT "**")
(ID "kwds")
(PUNCT ")")
(NEWLINE)
(KEYWORD return)
(ID "Process")
(PUNCT "(")
(ID "transport")
(PUNCT ",")
(ID "protocol")
(PUNCT ",")
(ID "loop")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ENDMARKER)
