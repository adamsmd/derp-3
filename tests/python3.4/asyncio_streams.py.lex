(LIT "Stream-related things.")
(NEWLINE)
(ID "__all__")
(PUNCT "=")
(PUNCT "[")
(LIT "StreamReader")
(PUNCT ",")
(LIT "StreamWriter")
(PUNCT ",")
(LIT "StreamReaderProtocol")
(PUNCT ",")
(LIT "open_connection")
(PUNCT ",")
(LIT "start_server")
(PUNCT ",")
(LIT "IncompleteReadError")
(PUNCT ",")
(PUNCT "]")
(NEWLINE)
(KEYWORD import)
(ID "socket")
(NEWLINE)
(KEYWORD if)
(ID "hasattr")
(PUNCT "(")
(ID "socket")
(PUNCT ",")
(LIT "AF_UNIX")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "__all__")
(PUNCT ".")
(ID "extend")
(PUNCT "(")
(PUNCT "[")
(LIT "open_unix_connection")
(PUNCT ",")
(LIT "start_unix_server")
(PUNCT "]")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD from)
(PUNCT ".")
(KEYWORD import)
(ID "coroutines")
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
(ID "_DEFAULT_LIMIT")
(PUNCT "=")
(LIT 2)
(PUNCT "**")
(LIT 16)
(NEWLINE)
(KEYWORD class)
(ID "IncompleteReadError")
(PUNCT "(")
(ID "EOFError")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "\n    Incomplete read error. Attributes:\n\n    - partial: read bytes string before the end of stream was reached\n    - expected: total number of expected bytes\n    ")
(NEWLINE)
(KEYWORD def)
(ID "__init__")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "partial")
(PUNCT ",")
(ID "expected")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "EOFError")
(PUNCT ".")
(ID "__init__")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(LIT "%s bytes read on a total of %s expected bytes")
(PUNCT "%")
(PUNCT "(")
(ID "len")
(PUNCT "(")
(ID "partial")
(PUNCT ")")
(PUNCT ",")
(ID "expected")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "partial")
(PUNCT "=")
(ID "partial")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "expected")
(PUNCT "=")
(ID "expected")
(NEWLINE)
(DEDENT)
(DEDENT)
(PUNCT "@")
(ID "coroutine")
(NEWLINE)
(KEYWORD def)
(ID "open_connection")
(PUNCT "(")
(ID "host")
(PUNCT "=")
(KEYWORD None)
(PUNCT ",")
(ID "port")
(PUNCT "=")
(KEYWORD None)
(PUNCT ",")
(PUNCT "*")
(PUNCT ",")
(ID "loop")
(PUNCT "=")
(KEYWORD None)
(PUNCT ",")
(ID "limit")
(PUNCT "=")
(ID "_DEFAULT_LIMIT")
(PUNCT ",")
(PUNCT "**")
(ID "kwds")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "A wrapper for create_connection() returning a (reader, writer) pair.\n\n    The reader returned is a StreamReader instance; the writer is a\n    StreamWriter instance.\n\n    The arguments are all the usual arguments to create_connection()\n    except protocol_factory; most common are positional host and port,\n    with various optional keyword arguments following.\n\n    Additional optional keyword arguments are loop (to set the event loop\n    instance to use) and limit (to set the buffer limit passed to the\n    StreamReader).\n\n    (If you want to customize the StreamReader and/or\n    StreamReaderProtocol classes, just copy the code -- there's\n    really nothing special here except some convenience.)\n    ")
(NEWLINE)
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
(ID "reader")
(PUNCT "=")
(ID "StreamReader")
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
(ID "protocol")
(PUNCT "=")
(ID "StreamReaderProtocol")
(PUNCT "(")
(ID "reader")
(PUNCT ",")
(ID "loop")
(PUNCT "=")
(ID "loop")
(PUNCT ")")
(NEWLINE)
(ID "transport")
(PUNCT ",")
(ID "_")
(PUNCT "=")
(KEYWORD yield)
(KEYWORD from)
(ID "loop")
(PUNCT ".")
(ID "create_connection")
(PUNCT "(")
(KEYWORD lambda)
(PUNCT ":")
(ID "protocol")
(PUNCT ",")
(ID "host")
(PUNCT ",")
(ID "port")
(PUNCT ",")
(PUNCT "**")
(ID "kwds")
(PUNCT ")")
(NEWLINE)
(ID "writer")
(PUNCT "=")
(ID "StreamWriter")
(PUNCT "(")
(ID "transport")
(PUNCT ",")
(ID "protocol")
(PUNCT ",")
(ID "reader")
(PUNCT ",")
(ID "loop")
(PUNCT ")")
(NEWLINE)
(KEYWORD return)
(ID "reader")
(PUNCT ",")
(ID "writer")
(NEWLINE)
(DEDENT)
(PUNCT "@")
(ID "coroutine")
(NEWLINE)
(KEYWORD def)
(ID "start_server")
(PUNCT "(")
(ID "client_connected_cb")
(PUNCT ",")
(ID "host")
(PUNCT "=")
(KEYWORD None)
(PUNCT ",")
(ID "port")
(PUNCT "=")
(KEYWORD None)
(PUNCT ",")
(PUNCT "*")
(PUNCT ",")
(ID "loop")
(PUNCT "=")
(KEYWORD None)
(PUNCT ",")
(ID "limit")
(PUNCT "=")
(ID "_DEFAULT_LIMIT")
(PUNCT ",")
(PUNCT "**")
(ID "kwds")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Start a socket server, call back for each client connected.\n\n    The first parameter, `client_connected_cb`, takes two parameters:\n    client_reader, client_writer.  client_reader is a StreamReader\n    object, while client_writer is a StreamWriter object.  This\n    parameter can either be a plain callback function or a coroutine;\n    if it is a coroutine, it will be automatically converted into a\n    Task.\n\n    The rest of the arguments are all the usual arguments to\n    loop.create_server() except protocol_factory; most common are\n    positional host and port, with various optional keyword arguments\n    following.  The return value is the same as loop.create_server().\n\n    Additional optional keyword arguments are loop (to set the event loop\n    instance to use) and limit (to set the buffer limit passed to the\n    StreamReader).\n\n    The return value is the same as loop.create_server(), i.e. a\n    Server object which can be used to stop the service.\n    ")
(NEWLINE)
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
(KEYWORD def)
(ID "factory")
(PUNCT "(")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "reader")
(PUNCT "=")
(ID "StreamReader")
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
(ID "protocol")
(PUNCT "=")
(ID "StreamReaderProtocol")
(PUNCT "(")
(ID "reader")
(PUNCT ",")
(ID "client_connected_cb")
(PUNCT ",")
(ID "loop")
(PUNCT "=")
(ID "loop")
(PUNCT ")")
(NEWLINE)
(KEYWORD return)
(ID "protocol")
(NEWLINE)
(DEDENT)
(KEYWORD return)
(PUNCT "(")
(KEYWORD yield)
(KEYWORD from)
(ID "loop")
(PUNCT ".")
(ID "create_server")
(PUNCT "(")
(ID "factory")
(PUNCT ",")
(ID "host")
(PUNCT ",")
(ID "port")
(PUNCT ",")
(PUNCT "**")
(ID "kwds")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD if)
(ID "hasattr")
(PUNCT "(")
(ID "socket")
(PUNCT ",")
(LIT "AF_UNIX")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(PUNCT "@")
(ID "coroutine")
(NEWLINE)
(KEYWORD def)
(ID "open_unix_connection")
(PUNCT "(")
(ID "path")
(PUNCT "=")
(KEYWORD None)
(PUNCT ",")
(PUNCT "*")
(PUNCT ",")
(ID "loop")
(PUNCT "=")
(KEYWORD None)
(PUNCT ",")
(ID "limit")
(PUNCT "=")
(ID "_DEFAULT_LIMIT")
(PUNCT ",")
(PUNCT "**")
(ID "kwds")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Similar to `open_connection` but works with UNIX Domain Sockets.")
(NEWLINE)
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
(ID "reader")
(PUNCT "=")
(ID "StreamReader")
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
(ID "protocol")
(PUNCT "=")
(ID "StreamReaderProtocol")
(PUNCT "(")
(ID "reader")
(PUNCT ",")
(ID "loop")
(PUNCT "=")
(ID "loop")
(PUNCT ")")
(NEWLINE)
(ID "transport")
(PUNCT ",")
(ID "_")
(PUNCT "=")
(KEYWORD yield)
(KEYWORD from)
(ID "loop")
(PUNCT ".")
(ID "create_unix_connection")
(PUNCT "(")
(KEYWORD lambda)
(PUNCT ":")
(ID "protocol")
(PUNCT ",")
(ID "path")
(PUNCT ",")
(PUNCT "**")
(ID "kwds")
(PUNCT ")")
(NEWLINE)
(ID "writer")
(PUNCT "=")
(ID "StreamWriter")
(PUNCT "(")
(ID "transport")
(PUNCT ",")
(ID "protocol")
(PUNCT ",")
(ID "reader")
(PUNCT ",")
(ID "loop")
(PUNCT ")")
(NEWLINE)
(KEYWORD return)
(ID "reader")
(PUNCT ",")
(ID "writer")
(NEWLINE)
(DEDENT)
(PUNCT "@")
(ID "coroutine")
(NEWLINE)
(KEYWORD def)
(ID "start_unix_server")
(PUNCT "(")
(ID "client_connected_cb")
(PUNCT ",")
(ID "path")
(PUNCT "=")
(KEYWORD None)
(PUNCT ",")
(PUNCT "*")
(PUNCT ",")
(ID "loop")
(PUNCT "=")
(KEYWORD None)
(PUNCT ",")
(ID "limit")
(PUNCT "=")
(ID "_DEFAULT_LIMIT")
(PUNCT ",")
(PUNCT "**")
(ID "kwds")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Similar to `start_server` but works with UNIX Domain Sockets.")
(NEWLINE)
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
(KEYWORD def)
(ID "factory")
(PUNCT "(")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "reader")
(PUNCT "=")
(ID "StreamReader")
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
(ID "protocol")
(PUNCT "=")
(ID "StreamReaderProtocol")
(PUNCT "(")
(ID "reader")
(PUNCT ",")
(ID "client_connected_cb")
(PUNCT ",")
(ID "loop")
(PUNCT "=")
(ID "loop")
(PUNCT ")")
(NEWLINE)
(KEYWORD return)
(ID "protocol")
(NEWLINE)
(DEDENT)
(KEYWORD return)
(PUNCT "(")
(KEYWORD yield)
(KEYWORD from)
(ID "loop")
(PUNCT ".")
(ID "create_unix_server")
(PUNCT "(")
(ID "factory")
(PUNCT ",")
(ID "path")
(PUNCT ",")
(PUNCT "**")
(ID "kwds")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD class)
(ID "FlowControlMixin")
(PUNCT "(")
(ID "protocols")
(PUNCT ".")
(ID "Protocol")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Reusable flow control logic for StreamWriter.drain().\n\n    This implements the protocol methods pause_writing(),\n    resume_reading() and connection_lost().  If the subclass overrides\n    these it must call the super methods.\n\n    StreamWriter.drain() must wait for _drain_helper() coroutine.\n    ")
(NEWLINE)
(KEYWORD def)
(ID "__init__")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "loop")
(PUNCT "=")
(KEYWORD None)
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
(ID "self")
(PUNCT ".")
(ID "_loop")
(PUNCT "=")
(ID "events")
(PUNCT ".")
(ID "get_event_loop")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "_loop")
(PUNCT "=")
(ID "loop")
(NEWLINE)
(DEDENT)
(ID "self")
(PUNCT ".")
(ID "_paused")
(PUNCT "=")
(KEYWORD False)
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_drain_waiter")
(PUNCT "=")
(KEYWORD None)
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_connection_lost")
(PUNCT "=")
(KEYWORD False)
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "pause_writing")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD assert)
(KEYWORD not)
(ID "self")
(PUNCT ".")
(ID "_paused")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_paused")
(PUNCT "=")
(KEYWORD True)
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
(ID "logger")
(PUNCT ".")
(ID "debug")
(PUNCT "(")
(LIT "%r pauses writing")
(PUNCT ",")
(ID "self")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD def)
(ID "resume_writing")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD assert)
(ID "self")
(PUNCT ".")
(ID "_paused")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_paused")
(PUNCT "=")
(KEYWORD False)
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
(ID "logger")
(PUNCT ".")
(ID "debug")
(PUNCT "(")
(LIT "%r resumes writing")
(PUNCT ",")
(ID "self")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "waiter")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "_drain_waiter")
(NEWLINE)
(KEYWORD if)
(ID "waiter")
(KEYWORD is)
(KEYWORD not)
(KEYWORD None)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "_drain_waiter")
(PUNCT "=")
(KEYWORD None)
(NEWLINE)
(KEYWORD if)
(KEYWORD not)
(ID "waiter")
(PUNCT ".")
(ID "done")
(PUNCT "(")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "waiter")
(PUNCT ".")
(ID "set_result")
(PUNCT "(")
(KEYWORD None)
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(DEDENT)
(KEYWORD def)
(ID "connection_lost")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "exc")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "_connection_lost")
(PUNCT "=")
(KEYWORD True)
(NEWLINE)
(KEYWORD if)
(KEYWORD not)
(ID "self")
(PUNCT ".")
(ID "_paused")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(NEWLINE)
(DEDENT)
(ID "waiter")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "_drain_waiter")
(NEWLINE)
(KEYWORD if)
(ID "waiter")
(KEYWORD is)
(KEYWORD None)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(NEWLINE)
(DEDENT)
(ID "self")
(PUNCT ".")
(ID "_drain_waiter")
(PUNCT "=")
(KEYWORD None)
(NEWLINE)
(KEYWORD if)
(ID "waiter")
(PUNCT ".")
(ID "done")
(PUNCT "(")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(NEWLINE)
(DEDENT)
(KEYWORD if)
(ID "exc")
(KEYWORD is)
(KEYWORD None)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "waiter")
(PUNCT ".")
(ID "set_result")
(PUNCT "(")
(KEYWORD None)
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "waiter")
(PUNCT ".")
(ID "set_exception")
(PUNCT "(")
(ID "exc")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(PUNCT "@")
(ID "coroutine")
(NEWLINE)
(KEYWORD def)
(ID "_drain_helper")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "self")
(PUNCT ".")
(ID "_connection_lost")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD raise)
(ID "ConnectionResetError")
(PUNCT "(")
(LIT "Connection lost")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD if)
(KEYWORD not)
(ID "self")
(PUNCT ".")
(ID "_paused")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(NEWLINE)
(DEDENT)
(ID "waiter")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "_drain_waiter")
(NEWLINE)
(KEYWORD assert)
(ID "waiter")
(KEYWORD is)
(KEYWORD None)
(KEYWORD or)
(ID "waiter")
(PUNCT ".")
(ID "cancelled")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "waiter")
(PUNCT "=")
(ID "futures")
(PUNCT ".")
(ID "Future")
(PUNCT "(")
(ID "loop")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "_loop")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_drain_waiter")
(PUNCT "=")
(ID "waiter")
(NEWLINE)
(KEYWORD yield)
(KEYWORD from)
(ID "waiter")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD class)
(ID "StreamReaderProtocol")
(PUNCT "(")
(ID "FlowControlMixin")
(PUNCT ",")
(ID "protocols")
(PUNCT ".")
(ID "Protocol")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Helper class to adapt between Protocol and StreamReader.\n\n    (This is a helper class instead of making StreamReader itself a\n    Protocol subclass, because the StreamReader has other potential\n    uses, and to prevent the user of the StreamReader to accidentally\n    call inappropriate methods of the protocol.)\n    ")
(NEWLINE)
(KEYWORD def)
(ID "__init__")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "stream_reader")
(PUNCT ",")
(ID "client_connected_cb")
(PUNCT "=")
(KEYWORD None)
(PUNCT ",")
(ID "loop")
(PUNCT "=")
(KEYWORD None)
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
(ID "_stream_reader")
(PUNCT "=")
(ID "stream_reader")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_stream_writer")
(PUNCT "=")
(KEYWORD None)
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_client_connected_cb")
(PUNCT "=")
(ID "client_connected_cb")
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
(ID "_stream_reader")
(PUNCT ".")
(ID "set_transport")
(PUNCT "(")
(ID "transport")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(ID "self")
(PUNCT ".")
(ID "_client_connected_cb")
(KEYWORD is)
(KEYWORD not)
(KEYWORD None)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "_stream_writer")
(PUNCT "=")
(ID "StreamWriter")
(PUNCT "(")
(ID "transport")
(PUNCT ",")
(ID "self")
(PUNCT ",")
(ID "self")
(PUNCT ".")
(ID "_stream_reader")
(PUNCT ",")
(ID "self")
(PUNCT ".")
(ID "_loop")
(PUNCT ")")
(NEWLINE)
(ID "res")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "_client_connected_cb")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "_stream_reader")
(PUNCT ",")
(ID "self")
(PUNCT ".")
(ID "_stream_writer")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(ID "coroutines")
(PUNCT ".")
(ID "iscoroutine")
(PUNCT "(")
(ID "res")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "_loop")
(PUNCT ".")
(ID "create_task")
(PUNCT "(")
(ID "res")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(DEDENT)
(KEYWORD def)
(ID "connection_lost")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "exc")
(PUNCT ")")
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
(ID "self")
(PUNCT ".")
(ID "_stream_reader")
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
(ID "self")
(PUNCT ".")
(ID "_stream_reader")
(PUNCT ".")
(ID "set_exception")
(PUNCT "(")
(ID "exc")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "super")
(PUNCT "(")
(PUNCT ")")
(PUNCT ".")
(ID "connection_lost")
(PUNCT "(")
(ID "exc")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "data_received")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "data")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "_stream_reader")
(PUNCT ".")
(ID "feed_data")
(PUNCT "(")
(ID "data")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "eof_received")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "_stream_reader")
(PUNCT ".")
(ID "feed_eof")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD class)
(ID "StreamWriter")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Wraps a Transport.\n\n    This exposes write(), writelines(), [can_]write_eof(),\n    get_extra_info() and close().  It adds drain() which returns an\n    optional Future on which you can wait for flow control.  It also\n    adds a transport property which references the Transport\n    directly.\n    ")
(NEWLINE)
(KEYWORD def)
(ID "__init__")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "transport")
(PUNCT ",")
(ID "protocol")
(PUNCT ",")
(ID "reader")
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
(KEYWORD assert)
(ID "reader")
(KEYWORD is)
(KEYWORD None)
(KEYWORD or)
(ID "isinstance")
(PUNCT "(")
(ID "reader")
(PUNCT ",")
(ID "StreamReader")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_reader")
(PUNCT "=")
(ID "reader")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_loop")
(PUNCT "=")
(ID "loop")
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
(PUNCT ",")
(LIT "transport=%r")
(PUNCT "%")
(ID "self")
(PUNCT ".")
(ID "_transport")
(PUNCT "]")
(NEWLINE)
(KEYWORD if)
(ID "self")
(PUNCT ".")
(ID "_reader")
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
(LIT "reader=%r")
(PUNCT "%")
(ID "self")
(PUNCT ".")
(ID "_reader")
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
(PUNCT "@")
(ID "property")
(NEWLINE)
(KEYWORD def)
(ID "transport")
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
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "write")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "data")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "_transport")
(PUNCT ".")
(ID "write")
(PUNCT "(")
(ID "data")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "writelines")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "data")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "_transport")
(PUNCT ".")
(ID "writelines")
(PUNCT "(")
(ID "data")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "write_eof")
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
(ID "write_eof")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "can_write_eof")
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
(ID "can_write_eof")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "close")
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
(ID "close")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "get_extra_info")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "name")
(PUNCT ",")
(ID "default")
(PUNCT "=")
(KEYWORD None)
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(ID "self")
(PUNCT ".")
(ID "_transport")
(PUNCT ".")
(ID "get_extra_info")
(PUNCT "(")
(ID "name")
(PUNCT ",")
(ID "default")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(PUNCT "@")
(ID "coroutine")
(NEWLINE)
(KEYWORD def)
(ID "drain")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Flush the write buffer.\n\n        The intended use is to write\n\n          w.write(data)\n          yield from w.drain()\n        ")
(NEWLINE)
(KEYWORD if)
(ID "self")
(PUNCT ".")
(ID "_reader")
(KEYWORD is)
(KEYWORD not)
(KEYWORD None)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "exc")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "_reader")
(PUNCT ".")
(ID "exception")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(ID "exc")
(KEYWORD is)
(KEYWORD not)
(KEYWORD None)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD raise)
(ID "exc")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD yield)
(KEYWORD from)
(ID "self")
(PUNCT ".")
(ID "_protocol")
(PUNCT ".")
(ID "_drain_helper")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD class)
(ID "StreamReader")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD def)
(ID "__init__")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "limit")
(PUNCT "=")
(ID "_DEFAULT_LIMIT")
(PUNCT ",")
(ID "loop")
(PUNCT "=")
(KEYWORD None)
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "_limit")
(PUNCT "=")
(ID "limit")
(NEWLINE)
(KEYWORD if)
(ID "loop")
(KEYWORD is)
(KEYWORD None)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "_loop")
(PUNCT "=")
(ID "events")
(PUNCT ".")
(ID "get_event_loop")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "_loop")
(PUNCT "=")
(ID "loop")
(NEWLINE)
(DEDENT)
(ID "self")
(PUNCT ".")
(ID "_buffer")
(PUNCT "=")
(ID "bytearray")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_eof")
(PUNCT "=")
(KEYWORD False)
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_waiter")
(PUNCT "=")
(KEYWORD None)
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_exception")
(PUNCT "=")
(KEYWORD None)
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_transport")
(PUNCT "=")
(KEYWORD None)
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_paused")
(PUNCT "=")
(KEYWORD False)
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "exception")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(ID "self")
(PUNCT ".")
(ID "_exception")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "set_exception")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "exc")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "_exception")
(PUNCT "=")
(ID "exc")
(NEWLINE)
(ID "waiter")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "_waiter")
(NEWLINE)
(KEYWORD if)
(ID "waiter")
(KEYWORD is)
(KEYWORD not)
(KEYWORD None)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "_waiter")
(PUNCT "=")
(KEYWORD None)
(NEWLINE)
(KEYWORD if)
(KEYWORD not)
(ID "waiter")
(PUNCT ".")
(ID "cancelled")
(PUNCT "(")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "waiter")
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
(ID "_wakeup_waiter")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Wakeup read() or readline() function waiting for data or EOF.")
(NEWLINE)
(ID "waiter")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "_waiter")
(NEWLINE)
(KEYWORD if)
(ID "waiter")
(KEYWORD is)
(KEYWORD not)
(KEYWORD None)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "_waiter")
(PUNCT "=")
(KEYWORD None)
(NEWLINE)
(KEYWORD if)
(KEYWORD not)
(ID "waiter")
(PUNCT ".")
(ID "cancelled")
(PUNCT "(")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "waiter")
(PUNCT ".")
(ID "set_result")
(PUNCT "(")
(KEYWORD None)
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(DEDENT)
(KEYWORD def)
(ID "set_transport")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "transport")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD assert)
(ID "self")
(PUNCT ".")
(ID "_transport")
(KEYWORD is)
(KEYWORD None)
(PUNCT ",")
(LIT "Transport already set")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_transport")
(PUNCT "=")
(ID "transport")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "_maybe_resume_transport")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "self")
(PUNCT ".")
(ID "_paused")
(KEYWORD and)
(ID "len")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "_buffer")
(PUNCT ")")
(PUNCT "<=")
(ID "self")
(PUNCT ".")
(ID "_limit")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "_paused")
(PUNCT "=")
(KEYWORD False)
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_transport")
(PUNCT ".")
(ID "resume_reading")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD def)
(ID "feed_eof")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "_eof")
(PUNCT "=")
(KEYWORD True)
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_wakeup_waiter")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "at_eof")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Return True if the buffer is empty and 'feed_eof' was called.")
(NEWLINE)
(KEYWORD return)
(ID "self")
(PUNCT ".")
(ID "_eof")
(KEYWORD and)
(KEYWORD not)
(ID "self")
(PUNCT ".")
(ID "_buffer")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "feed_data")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "data")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD assert)
(KEYWORD not)
(ID "self")
(PUNCT ".")
(ID "_eof")
(PUNCT ",")
(LIT "feed_data after feed_eof")
(NEWLINE)
(KEYWORD if)
(KEYWORD not)
(ID "data")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(NEWLINE)
(DEDENT)
(ID "self")
(PUNCT ".")
(ID "_buffer")
(PUNCT ".")
(ID "extend")
(PUNCT "(")
(ID "data")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_wakeup_waiter")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "_transport")
(KEYWORD is)
(KEYWORD not)
(KEYWORD None)
(KEYWORD and)
(KEYWORD not)
(ID "self")
(PUNCT ".")
(ID "_paused")
(KEYWORD and)
(ID "len")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "_buffer")
(PUNCT ")")
(PUNCT ">")
(LIT 2)
(PUNCT "*")
(ID "self")
(PUNCT ".")
(ID "_limit")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD try)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "_transport")
(PUNCT ".")
(ID "pause_reading")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD except)
(ID "NotImplementedError")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "_transport")
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
(ID "_paused")
(PUNCT "=")
(KEYWORD True)
(NEWLINE)
(DEDENT)
(DEDENT)
(DEDENT)
(KEYWORD def)
(ID "_wait_for_data")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "func_name")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Wait until feed_data() or feed_eof() is called.")
(NEWLINE)
(KEYWORD if)
(ID "self")
(PUNCT ".")
(ID "_waiter")
(KEYWORD is)
(KEYWORD not)
(KEYWORD None)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD raise)
(ID "RuntimeError")
(PUNCT "(")
(LIT "%s() called while another coroutine is ")
(LIT "already waiting for incoming data")
(PUNCT "%")
(ID "func_name")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "self")
(PUNCT ".")
(ID "_waiter")
(PUNCT "=")
(ID "futures")
(PUNCT ".")
(ID "Future")
(PUNCT "(")
(ID "loop")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "_loop")
(PUNCT ")")
(NEWLINE)
(KEYWORD try)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD yield)
(KEYWORD from)
(ID "self")
(PUNCT ".")
(ID "_waiter")
(NEWLINE)
(DEDENT)
(KEYWORD finally)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "_waiter")
(PUNCT "=")
(KEYWORD None)
(NEWLINE)
(DEDENT)
(DEDENT)
(PUNCT "@")
(ID "coroutine")
(NEWLINE)
(KEYWORD def)
(ID "readline")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "self")
(PUNCT ".")
(ID "_exception")
(KEYWORD is)
(KEYWORD not)
(KEYWORD None)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD raise)
(ID "self")
(PUNCT ".")
(ID "_exception")
(NEWLINE)
(DEDENT)
(ID "line")
(PUNCT "=")
(ID "bytearray")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "not_enough")
(PUNCT "=")
(KEYWORD True)
(NEWLINE)
(KEYWORD while)
(ID "not_enough")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD while)
(ID "self")
(PUNCT ".")
(ID "_buffer")
(KEYWORD and)
(ID "not_enough")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "ichar")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "_buffer")
(PUNCT ".")
(ID "find")
(PUNCT "(")
(LIT #"\n")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(ID "ichar")
(PUNCT "<")
(LIT 0)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "line")
(PUNCT ".")
(ID "extend")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "_buffer")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_buffer")
(PUNCT ".")
(ID "clear")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "ichar")
(PUNCT "+=")
(LIT 1)
(NEWLINE)
(ID "line")
(PUNCT ".")
(ID "extend")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "_buffer")
(PUNCT "[")
(PUNCT ":")
(ID "ichar")
(PUNCT "]")
(PUNCT ")")
(NEWLINE)
(KEYWORD del)
(ID "self")
(PUNCT ".")
(ID "_buffer")
(PUNCT "[")
(PUNCT ":")
(ID "ichar")
(PUNCT "]")
(NEWLINE)
(ID "not_enough")
(PUNCT "=")
(KEYWORD False)
(NEWLINE)
(DEDENT)
(KEYWORD if)
(ID "len")
(PUNCT "(")
(ID "line")
(PUNCT ")")
(PUNCT ">")
(ID "self")
(PUNCT ".")
(ID "_limit")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "_maybe_resume_transport")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(KEYWORD raise)
(ID "ValueError")
(PUNCT "(")
(LIT "Line is too long")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD if)
(ID "self")
(PUNCT ".")
(ID "_eof")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD break)
(NEWLINE)
(DEDENT)
(KEYWORD if)
(ID "not_enough")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD yield)
(KEYWORD from)
(ID "self")
(PUNCT ".")
(ID "_wait_for_data")
(PUNCT "(")
(LIT "readline")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(ID "self")
(PUNCT ".")
(ID "_maybe_resume_transport")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(KEYWORD return)
(ID "bytes")
(PUNCT "(")
(ID "line")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(PUNCT "@")
(ID "coroutine")
(NEWLINE)
(KEYWORD def)
(ID "read")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "n")
(PUNCT "=")
(PUNCT "-")
(LIT 1)
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "self")
(PUNCT ".")
(ID "_exception")
(KEYWORD is)
(KEYWORD not)
(KEYWORD None)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD raise)
(ID "self")
(PUNCT ".")
(ID "_exception")
(NEWLINE)
(DEDENT)
(KEYWORD if)
(KEYWORD not)
(ID "n")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(LIT #"")
(NEWLINE)
(DEDENT)
(KEYWORD if)
(ID "n")
(PUNCT "<")
(LIT 0)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "blocks")
(PUNCT "=")
(PUNCT "[")
(PUNCT "]")
(NEWLINE)
(KEYWORD while)
(KEYWORD True)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "block")
(PUNCT "=")
(KEYWORD yield)
(KEYWORD from)
(ID "self")
(PUNCT ".")
(ID "read")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "_limit")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(KEYWORD not)
(ID "block")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD break)
(NEWLINE)
(DEDENT)
(ID "blocks")
(PUNCT ".")
(ID "append")
(PUNCT "(")
(ID "block")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD return)
(LIT #"")
(PUNCT ".")
(ID "join")
(PUNCT "(")
(ID "blocks")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(KEYWORD not)
(ID "self")
(PUNCT ".")
(ID "_buffer")
(KEYWORD and)
(KEYWORD not)
(ID "self")
(PUNCT ".")
(ID "_eof")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD yield)
(KEYWORD from)
(ID "self")
(PUNCT ".")
(ID "_wait_for_data")
(PUNCT "(")
(LIT "read")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD if)
(ID "n")
(PUNCT "<")
(LIT 0)
(KEYWORD or)
(ID "len")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "_buffer")
(PUNCT ")")
(PUNCT "<=")
(ID "n")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "data")
(PUNCT "=")
(ID "bytes")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "_buffer")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_buffer")
(PUNCT ".")
(ID "clear")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "data")
(PUNCT "=")
(ID "bytes")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "_buffer")
(PUNCT "[")
(PUNCT ":")
(ID "n")
(PUNCT "]")
(PUNCT ")")
(NEWLINE)
(KEYWORD del)
(ID "self")
(PUNCT ".")
(ID "_buffer")
(PUNCT "[")
(PUNCT ":")
(ID "n")
(PUNCT "]")
(NEWLINE)
(DEDENT)
(ID "self")
(PUNCT ".")
(ID "_maybe_resume_transport")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(KEYWORD return)
(ID "data")
(NEWLINE)
(DEDENT)
(PUNCT "@")
(ID "coroutine")
(NEWLINE)
(KEYWORD def)
(ID "readexactly")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "n")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "self")
(PUNCT ".")
(ID "_exception")
(KEYWORD is)
(KEYWORD not)
(KEYWORD None)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD raise)
(ID "self")
(PUNCT ".")
(ID "_exception")
(NEWLINE)
(DEDENT)
(ID "blocks")
(PUNCT "=")
(PUNCT "[")
(PUNCT "]")
(NEWLINE)
(KEYWORD while)
(ID "n")
(PUNCT ">")
(LIT 0)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "block")
(PUNCT "=")
(KEYWORD yield)
(KEYWORD from)
(ID "self")
(PUNCT ".")
(ID "read")
(PUNCT "(")
(ID "n")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(KEYWORD not)
(ID "block")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "partial")
(PUNCT "=")
(LIT #"")
(PUNCT ".")
(ID "join")
(PUNCT "(")
(ID "blocks")
(PUNCT ")")
(NEWLINE)
(KEYWORD raise)
(ID "IncompleteReadError")
(PUNCT "(")
(ID "partial")
(PUNCT ",")
(ID "len")
(PUNCT "(")
(ID "partial")
(PUNCT ")")
(PUNCT "+")
(ID "n")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "blocks")
(PUNCT ".")
(ID "append")
(PUNCT "(")
(ID "block")
(PUNCT ")")
(NEWLINE)
(ID "n")
(PUNCT "-=")
(ID "len")
(PUNCT "(")
(ID "block")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD return)
(LIT #"")
(PUNCT ".")
(ID "join")
(PUNCT "(")
(ID "blocks")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(ENDMARKER)
