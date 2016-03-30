(KEYWORD import)
(ID "os")
(NEWLINE)
(KEYWORD import)
(ID "signal")
(NEWLINE)
(KEYWORD import)
(ID "socket")
(NEWLINE)
(KEYWORD import)
(ID "sys")
(NEWLINE)
(KEYWORD import)
(ID "threading")
(NEWLINE)
(KEYWORD from)
(PUNCT ".")
(KEYWORD import)
(ID "process")
(NEWLINE)
(KEYWORD from)
(PUNCT ".")
(KEYWORD import)
(ID "reduction")
(NEWLINE)
(KEYWORD from)
(PUNCT ".")
(KEYWORD import)
(ID "util")
(NEWLINE)
(ID "__all__")
(PUNCT "=")
(PUNCT "[")
(LIT "stop")
(PUNCT "]")
(NEWLINE)
(KEYWORD if)
(ID "sys")
(PUNCT ".")
(ID "platform")
(PUNCT "==")
(LIT "win32")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "__all__")
(PUNCT "+=")
(PUNCT "[")
(LIT "DupSocket")
(PUNCT "]")
(NEWLINE)
(KEYWORD class)
(ID "DupSocket")
(PUNCT "(")
(ID "object")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Picklable wrapper for a socket.")
(NEWLINE)
(KEYWORD def)
(ID "__init__")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "sock")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "new_sock")
(PUNCT "=")
(ID "sock")
(PUNCT ".")
(ID "dup")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(KEYWORD def)
(ID "send")
(PUNCT "(")
(ID "conn")
(PUNCT ",")
(ID "pid")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "share")
(PUNCT "=")
(ID "new_sock")
(PUNCT ".")
(ID "share")
(PUNCT "(")
(ID "pid")
(PUNCT ")")
(NEWLINE)
(ID "conn")
(PUNCT ".")
(ID "send_bytes")
(PUNCT "(")
(ID "share")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "self")
(PUNCT ".")
(ID "_id")
(PUNCT "=")
(ID "_resource_sharer")
(PUNCT ".")
(ID "register")
(PUNCT "(")
(ID "send")
(PUNCT ",")
(ID "new_sock")
(PUNCT ".")
(ID "close")
(PUNCT ")")
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
(LIT "Get the socket.  This should only be called once.")
(NEWLINE)
(KEYWORD with)
(ID "_resource_sharer")
(PUNCT ".")
(ID "get_connection")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "_id")
(PUNCT ")")
(KEYWORD as)
(ID "conn")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "share")
(PUNCT "=")
(ID "conn")
(PUNCT ".")
(ID "recv_bytes")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(KEYWORD return)
(ID "socket")
(PUNCT ".")
(ID "fromshare")
(PUNCT "(")
(ID "share")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(DEDENT)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "__all__")
(PUNCT "+=")
(PUNCT "[")
(LIT "DupFd")
(PUNCT "]")
(NEWLINE)
(KEYWORD class)
(ID "DupFd")
(PUNCT "(")
(ID "object")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Wrapper for fd which can be used at any time.")
(NEWLINE)
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
(ID "new_fd")
(PUNCT "=")
(ID "os")
(PUNCT ".")
(ID "dup")
(PUNCT "(")
(ID "fd")
(PUNCT ")")
(NEWLINE)
(KEYWORD def)
(ID "send")
(PUNCT "(")
(ID "conn")
(PUNCT ",")
(ID "pid")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "reduction")
(PUNCT ".")
(ID "send_handle")
(PUNCT "(")
(ID "conn")
(PUNCT ",")
(ID "new_fd")
(PUNCT ",")
(ID "pid")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "close")
(PUNCT "(")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "os")
(PUNCT ".")
(ID "close")
(PUNCT "(")
(ID "new_fd")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "self")
(PUNCT ".")
(ID "_id")
(PUNCT "=")
(ID "_resource_sharer")
(PUNCT ".")
(ID "register")
(PUNCT "(")
(ID "send")
(PUNCT ",")
(ID "close")
(PUNCT ")")
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
(LIT "Get the fd.  This should only be called once.")
(NEWLINE)
(KEYWORD with)
(ID "_resource_sharer")
(PUNCT ".")
(ID "get_connection")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "_id")
(PUNCT ")")
(KEYWORD as)
(ID "conn")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(ID "reduction")
(PUNCT ".")
(ID "recv_handle")
(PUNCT "(")
(ID "conn")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(DEDENT)
(DEDENT)
(KEYWORD class)
(ID "_ResourceSharer")
(PUNCT "(")
(ID "object")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Manager for resouces using background thread.")
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
(ID "_key")
(PUNCT "=")
(LIT 0)
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_cache")
(PUNCT "=")
(PUNCT "{")
(PUNCT "}")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_old_locks")
(PUNCT "=")
(PUNCT "[")
(PUNCT "]")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_lock")
(PUNCT "=")
(ID "threading")
(PUNCT ".")
(ID "Lock")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_listener")
(PUNCT "=")
(KEYWORD None)
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_address")
(PUNCT "=")
(KEYWORD None)
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_thread")
(PUNCT "=")
(KEYWORD None)
(NEWLINE)
(ID "util")
(PUNCT ".")
(ID "register_after_fork")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "_ResourceSharer")
(PUNCT ".")
(ID "_afterfork")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "register")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "send")
(PUNCT ",")
(ID "close")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Register resource, returning an identifier.")
(NEWLINE)
(KEYWORD with)
(ID "self")
(PUNCT ".")
(ID "_lock")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "self")
(PUNCT ".")
(ID "_address")
(KEYWORD is)
(KEYWORD None)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "_start")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "self")
(PUNCT ".")
(ID "_key")
(PUNCT "+=")
(LIT 1)
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_cache")
(PUNCT "[")
(ID "self")
(PUNCT ".")
(ID "_key")
(PUNCT "]")
(PUNCT "=")
(PUNCT "(")
(ID "send")
(PUNCT ",")
(ID "close")
(PUNCT ")")
(NEWLINE)
(KEYWORD return)
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "_address")
(PUNCT ",")
(ID "self")
(PUNCT ".")
(ID "_key")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(PUNCT "@")
(ID "staticmethod")
(NEWLINE)
(KEYWORD def)
(ID "get_connection")
(PUNCT "(")
(ID "ident")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Return connection from which to receive identified resource.")
(NEWLINE)
(KEYWORD from)
(PUNCT ".")
(ID "connection")
(KEYWORD import)
(ID "Client")
(NEWLINE)
(ID "address")
(PUNCT ",")
(ID "key")
(PUNCT "=")
(ID "ident")
(NEWLINE)
(ID "c")
(PUNCT "=")
(ID "Client")
(PUNCT "(")
(ID "address")
(PUNCT ",")
(ID "authkey")
(PUNCT "=")
(ID "process")
(PUNCT ".")
(ID "current_process")
(PUNCT "(")
(PUNCT ")")
(PUNCT ".")
(ID "authkey")
(PUNCT ")")
(NEWLINE)
(ID "c")
(PUNCT ".")
(ID "send")
(PUNCT "(")
(PUNCT "(")
(ID "key")
(PUNCT ",")
(ID "os")
(PUNCT ".")
(ID "getpid")
(PUNCT "(")
(PUNCT ")")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(KEYWORD return)
(ID "c")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "stop")
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
(LIT "Stop the background thread and clear registered resources.")
(NEWLINE)
(KEYWORD from)
(PUNCT ".")
(ID "connection")
(KEYWORD import)
(ID "Client")
(NEWLINE)
(KEYWORD with)
(ID "self")
(PUNCT ".")
(ID "_lock")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "self")
(PUNCT ".")
(ID "_address")
(KEYWORD is)
(KEYWORD not)
(KEYWORD None)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "c")
(PUNCT "=")
(ID "Client")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "_address")
(PUNCT ",")
(ID "authkey")
(PUNCT "=")
(ID "process")
(PUNCT ".")
(ID "current_process")
(PUNCT "(")
(PUNCT ")")
(PUNCT ".")
(ID "authkey")
(PUNCT ")")
(NEWLINE)
(ID "c")
(PUNCT ".")
(ID "send")
(PUNCT "(")
(KEYWORD None)
(PUNCT ")")
(NEWLINE)
(ID "c")
(PUNCT ".")
(ID "close")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_thread")
(PUNCT ".")
(ID "join")
(PUNCT "(")
(ID "timeout")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(ID "self")
(PUNCT ".")
(ID "_thread")
(PUNCT ".")
(ID "is_alive")
(PUNCT "(")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "util")
(PUNCT ".")
(ID "sub_warning")
(PUNCT "(")
(LIT "_ResourceSharer thread did ")
(LIT "not stop when asked")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "self")
(PUNCT ".")
(ID "_listener")
(PUNCT ".")
(ID "close")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_thread")
(PUNCT "=")
(KEYWORD None)
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_address")
(PUNCT "=")
(KEYWORD None)
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_listener")
(PUNCT "=")
(KEYWORD None)
(NEWLINE)
(KEYWORD for)
(ID "key")
(PUNCT ",")
(PUNCT "(")
(ID "send")
(PUNCT ",")
(ID "close")
(PUNCT ")")
(KEYWORD in)
(ID "self")
(PUNCT ".")
(ID "_cache")
(PUNCT ".")
(ID "items")
(PUNCT "(")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "close")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "self")
(PUNCT ".")
(ID "_cache")
(PUNCT ".")
(ID "clear")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(DEDENT)
(KEYWORD def)
(ID "_afterfork")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD for)
(ID "key")
(PUNCT ",")
(PUNCT "(")
(ID "send")
(PUNCT ",")
(ID "close")
(PUNCT ")")
(KEYWORD in)
(ID "self")
(PUNCT ".")
(ID "_cache")
(PUNCT ".")
(ID "items")
(PUNCT "(")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "close")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "self")
(PUNCT ".")
(ID "_cache")
(PUNCT ".")
(ID "clear")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_old_locks")
(PUNCT ".")
(ID "append")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "_lock")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_lock")
(PUNCT "=")
(ID "threading")
(PUNCT ".")
(ID "Lock")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(ID "self")
(PUNCT ".")
(ID "_listener")
(KEYWORD is)
(KEYWORD not)
(KEYWORD None)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "_listener")
(PUNCT ".")
(ID "close")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "self")
(PUNCT ".")
(ID "_listener")
(PUNCT "=")
(KEYWORD None)
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_address")
(PUNCT "=")
(KEYWORD None)
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_thread")
(PUNCT "=")
(KEYWORD None)
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "_start")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD from)
(PUNCT ".")
(ID "connection")
(KEYWORD import)
(ID "Listener")
(NEWLINE)
(KEYWORD assert)
(ID "self")
(PUNCT ".")
(ID "_listener")
(KEYWORD is)
(KEYWORD None)
(NEWLINE)
(ID "util")
(PUNCT ".")
(ID "debug")
(PUNCT "(")
(LIT "starting listener and thread for sending handles")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_listener")
(PUNCT "=")
(ID "Listener")
(PUNCT "(")
(ID "authkey")
(PUNCT "=")
(ID "process")
(PUNCT ".")
(ID "current_process")
(PUNCT "(")
(PUNCT ")")
(PUNCT ".")
(ID "authkey")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_address")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "_listener")
(PUNCT ".")
(ID "address")
(NEWLINE)
(ID "t")
(PUNCT "=")
(ID "threading")
(PUNCT ".")
(ID "Thread")
(PUNCT "(")
(ID "target")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "_serve")
(PUNCT ")")
(NEWLINE)
(ID "t")
(PUNCT ".")
(ID "daemon")
(PUNCT "=")
(KEYWORD True)
(NEWLINE)
(ID "t")
(PUNCT ".")
(ID "start")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_thread")
(PUNCT "=")
(ID "t")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "_serve")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "hasattr")
(PUNCT "(")
(ID "signal")
(PUNCT ",")
(LIT "pthread_sigmask")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "signal")
(PUNCT ".")
(ID "pthread_sigmask")
(PUNCT "(")
(ID "signal")
(PUNCT ".")
(ID "SIG_BLOCK")
(PUNCT ",")
(ID "range")
(PUNCT "(")
(LIT 1)
(PUNCT ",")
(ID "signal")
(PUNCT ".")
(ID "NSIG")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD while)
(LIT 1)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD try)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD with)
(ID "self")
(PUNCT ".")
(ID "_listener")
(PUNCT ".")
(ID "accept")
(PUNCT "(")
(PUNCT ")")
(KEYWORD as)
(ID "conn")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "msg")
(PUNCT "=")
(ID "conn")
(PUNCT ".")
(ID "recv")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(ID "msg")
(KEYWORD is)
(KEYWORD None)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD break)
(NEWLINE)
(DEDENT)
(ID "key")
(PUNCT ",")
(ID "destination_pid")
(PUNCT "=")
(ID "msg")
(NEWLINE)
(ID "send")
(PUNCT ",")
(ID "close")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "_cache")
(PUNCT ".")
(ID "pop")
(PUNCT "(")
(ID "key")
(PUNCT ")")
(NEWLINE)
(KEYWORD try)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "send")
(PUNCT "(")
(ID "conn")
(PUNCT ",")
(ID "destination_pid")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD finally)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "close")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(DEDENT)
(KEYWORD except)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(KEYWORD not)
(ID "util")
(PUNCT ".")
(ID "is_exiting")
(PUNCT "(")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "sys")
(PUNCT ".")
(ID "excepthook")
(PUNCT "(")
(PUNCT "*")
(ID "sys")
(PUNCT ".")
(ID "exc_info")
(PUNCT "(")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(DEDENT)
(DEDENT)
(DEDENT)
(ID "_resource_sharer")
(PUNCT "=")
(ID "_ResourceSharer")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "stop")
(PUNCT "=")
(ID "_resource_sharer")
(PUNCT ".")
(ID "stop")
(NEWLINE)
(ENDMARKER)