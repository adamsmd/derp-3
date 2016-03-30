(LIT "Class for profiling Python code.")
(NEWLINE)
(KEYWORD import)
(ID "sys")
(NEWLINE)
(KEYWORD import)
(ID "os")
(NEWLINE)
(KEYWORD import)
(ID "time")
(NEWLINE)
(KEYWORD import)
(ID "marshal")
(NEWLINE)
(KEYWORD from)
(ID "optparse")
(KEYWORD import)
(ID "OptionParser")
(NEWLINE)
(ID "__all__")
(PUNCT "=")
(PUNCT "[")
(LIT "run")
(PUNCT ",")
(LIT "runctx")
(PUNCT ",")
(LIT "Profile")
(PUNCT "]")
(NEWLINE)
(KEYWORD class)
(ID "_Utils")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Support class for utility functions which are shared by\n    profile.py and cProfile.py modules.\n    Not supposed to be used directly.\n    ")
(NEWLINE)
(KEYWORD def)
(ID "__init__")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "profiler")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "profiler")
(PUNCT "=")
(ID "profiler")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "run")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "statement")
(PUNCT ",")
(ID "filename")
(PUNCT ",")
(ID "sort")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "prof")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "profiler")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(KEYWORD try)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "prof")
(PUNCT ".")
(ID "run")
(PUNCT "(")
(ID "statement")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD except)
(ID "SystemExit")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD pass)
(NEWLINE)
(DEDENT)
(KEYWORD finally)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "_show")
(PUNCT "(")
(ID "prof")
(PUNCT ",")
(ID "filename")
(PUNCT ",")
(ID "sort")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD def)
(ID "runctx")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "statement")
(PUNCT ",")
(ID "globals")
(PUNCT ",")
(ID "locals")
(PUNCT ",")
(ID "filename")
(PUNCT ",")
(ID "sort")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "prof")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "profiler")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(KEYWORD try)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "prof")
(PUNCT ".")
(ID "runctx")
(PUNCT "(")
(ID "statement")
(PUNCT ",")
(ID "globals")
(PUNCT ",")
(ID "locals")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD except)
(ID "SystemExit")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD pass)
(NEWLINE)
(DEDENT)
(KEYWORD finally)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "_show")
(PUNCT "(")
(ID "prof")
(PUNCT ",")
(ID "filename")
(PUNCT ",")
(ID "sort")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD def)
(ID "_show")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "prof")
(PUNCT ",")
(ID "filename")
(PUNCT ",")
(ID "sort")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "filename")
(KEYWORD is)
(KEYWORD not)
(KEYWORD None)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "prof")
(PUNCT ".")
(ID "dump_stats")
(PUNCT "(")
(ID "filename")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "prof")
(PUNCT ".")
(ID "print_stats")
(PUNCT "(")
(ID "sort")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(DEDENT)
(KEYWORD def)
(ID "run")
(PUNCT "(")
(ID "statement")
(PUNCT ",")
(ID "filename")
(PUNCT "=")
(KEYWORD None)
(PUNCT ",")
(ID "sort")
(PUNCT "=")
(PUNCT "-")
(LIT 1)
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Run statement under profiler optionally saving results in filename\n\n    This function takes a single argument that can be passed to the\n    \"exec\" statement, and an optional file name.  In all cases this\n    routine attempts to \"exec\" its first argument and gather profiling\n    statistics from the execution. If no file name is present, then this\n    function automatically prints a simple profiling report, sorted by the\n    standard name string (file/line/function-name) that is presented in\n    each line.\n    ")
(NEWLINE)
(KEYWORD return)
(ID "_Utils")
(PUNCT "(")
(ID "Profile")
(PUNCT ")")
(PUNCT ".")
(ID "run")
(PUNCT "(")
(ID "statement")
(PUNCT ",")
(ID "filename")
(PUNCT ",")
(ID "sort")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "runctx")
(PUNCT "(")
(ID "statement")
(PUNCT ",")
(ID "globals")
(PUNCT ",")
(ID "locals")
(PUNCT ",")
(ID "filename")
(PUNCT "=")
(KEYWORD None)
(PUNCT ",")
(ID "sort")
(PUNCT "=")
(PUNCT "-")
(LIT 1)
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Run statement under profiler, supplying your own globals and locals,\n    optionally saving results in filename.\n\n    statement and filename have the same semantics as profile.run\n    ")
(NEWLINE)
(KEYWORD return)
(ID "_Utils")
(PUNCT "(")
(ID "Profile")
(PUNCT ")")
(PUNCT ".")
(ID "runctx")
(PUNCT "(")
(ID "statement")
(PUNCT ",")
(ID "globals")
(PUNCT ",")
(ID "locals")
(PUNCT ",")
(ID "filename")
(PUNCT ",")
(ID "sort")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD class)
(ID "Profile")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Profiler class.\n\n    self.cur is always a tuple.  Each such tuple corresponds to a stack\n    frame that is currently active (self.cur[-2]).  The following are the\n    definitions of its members.  We use this external \"parallel stack\" to\n    avoid contaminating the program that we are profiling. (old profiler\n    used to write into the frames local dictionary!!) Derived classes\n    can change the definition of some entries, as long as they leave\n    [-2:] intact (frame and previous tuple).  In case an internal error is\n    detected, the -3 element is used as the function name.\n\n    [ 0] = Time that needs to be charged to the parent frame's function.\n           It is used so that a function call will not have to access the\n           timing data for the parent frame.\n    [ 1] = Total time spent in this frame's function, excluding time in\n           subfunctions (this latter is tallied in cur[2]).\n    [ 2] = Total time spent in subfunctions, excluding time executing the\n           frame's function (this latter is tallied in cur[1]).\n    [-3] = Name of the function that corresponds to this frame.\n    [-2] = Actual frame that we correspond to (used to sync exception handling).\n    [-1] = Our parent 6-tuple (corresponds to frame.f_back).\n\n    Timing data for each function is stored as a 5-tuple in the dictionary\n    self.timings[].  The index is always the name stored in self.cur[-3].\n    The following are the definitions of the members:\n\n    [0] = The number of times this function was called, not counting direct\n          or indirect recursion,\n    [1] = Number of times this function appears on the stack, minus one\n    [2] = Total time spent internal to this function\n    [3] = Cumulative time that this function was present on the stack.  In\n          non-recursive functions, this is the total execution time from start\n          to finish of each invocation of a function, including time spent in\n          all subfunctions.\n    [4] = A dictionary indicating for each function name, the number of times\n          it was called by us.\n    ")
(NEWLINE)
(ID "bias")
(PUNCT "=")
(LIT 0)
(NEWLINE)
(KEYWORD def)
(ID "__init__")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "timer")
(PUNCT "=")
(KEYWORD None)
(PUNCT ",")
(ID "bias")
(PUNCT "=")
(KEYWORD None)
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "timings")
(PUNCT "=")
(PUNCT "{")
(PUNCT "}")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "cur")
(PUNCT "=")
(KEYWORD None)
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "cmd")
(PUNCT "=")
(LIT "")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "c_func_name")
(PUNCT "=")
(LIT "")
(NEWLINE)
(KEYWORD if)
(ID "bias")
(KEYWORD is)
(KEYWORD None)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "bias")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "bias")
(NEWLINE)
(DEDENT)
(ID "self")
(PUNCT ".")
(ID "bias")
(PUNCT "=")
(ID "bias")
(NEWLINE)
(KEYWORD if)
(KEYWORD not)
(ID "timer")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "timer")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "get_time")
(PUNCT "=")
(ID "time")
(PUNCT ".")
(ID "process_time")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "dispatcher")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "trace_dispatch_i")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "timer")
(PUNCT "=")
(ID "timer")
(NEWLINE)
(ID "t")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "timer")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(KEYWORD try)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "length")
(PUNCT "=")
(ID "len")
(PUNCT "(")
(ID "t")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD except)
(ID "TypeError")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "get_time")
(PUNCT "=")
(ID "timer")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "dispatcher")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "trace_dispatch_i")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "length")
(PUNCT "==")
(LIT 2)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "dispatcher")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "trace_dispatch")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "dispatcher")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "trace_dispatch_l")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "get_time_timer")
(PUNCT "(")
(ID "timer")
(PUNCT "=")
(ID "timer")
(PUNCT ",")
(ID "sum")
(PUNCT "=")
(ID "sum")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(ID "sum")
(PUNCT "(")
(ID "timer")
(PUNCT "(")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "self")
(PUNCT ".")
(ID "get_time")
(PUNCT "=")
(ID "get_time_timer")
(NEWLINE)
(DEDENT)
(DEDENT)
(ID "self")
(PUNCT ".")
(ID "t")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "get_time")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "simulate_call")
(PUNCT "(")
(LIT "profiler")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "trace_dispatch")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "frame")
(PUNCT ",")
(ID "event")
(PUNCT ",")
(ID "arg")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "timer")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "timer")
(NEWLINE)
(ID "t")
(PUNCT "=")
(ID "timer")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "t")
(PUNCT "=")
(ID "t")
(PUNCT "[")
(LIT 0)
(PUNCT "]")
(PUNCT "+")
(ID "t")
(PUNCT "[")
(LIT 1)
(PUNCT "]")
(PUNCT "-")
(ID "self")
(PUNCT ".")
(ID "t")
(PUNCT "-")
(ID "self")
(PUNCT ".")
(ID "bias")
(NEWLINE)
(KEYWORD if)
(ID "event")
(PUNCT "==")
(LIT "c_call")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "c_func_name")
(PUNCT "=")
(ID "arg")
(PUNCT ".")
(ID "__name__")
(NEWLINE)
(DEDENT)
(KEYWORD if)
(ID "self")
(PUNCT ".")
(ID "dispatch")
(PUNCT "[")
(ID "event")
(PUNCT "]")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "frame")
(PUNCT ",")
(ID "t")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "t")
(PUNCT "=")
(ID "timer")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "t")
(PUNCT "=")
(ID "t")
(PUNCT "[")
(LIT 0)
(PUNCT "]")
(PUNCT "+")
(ID "t")
(PUNCT "[")
(LIT 1)
(PUNCT "]")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "r")
(PUNCT "=")
(ID "timer")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "t")
(PUNCT "=")
(ID "r")
(PUNCT "[")
(LIT 0)
(PUNCT "]")
(PUNCT "+")
(ID "r")
(PUNCT "[")
(LIT 1)
(PUNCT "]")
(PUNCT "-")
(ID "t")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD def)
(ID "trace_dispatch_i")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "frame")
(PUNCT ",")
(ID "event")
(PUNCT ",")
(ID "arg")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "timer")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "timer")
(NEWLINE)
(ID "t")
(PUNCT "=")
(ID "timer")
(PUNCT "(")
(PUNCT ")")
(PUNCT "-")
(ID "self")
(PUNCT ".")
(ID "t")
(PUNCT "-")
(ID "self")
(PUNCT ".")
(ID "bias")
(NEWLINE)
(KEYWORD if)
(ID "event")
(PUNCT "==")
(LIT "c_call")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "c_func_name")
(PUNCT "=")
(ID "arg")
(PUNCT ".")
(ID "__name__")
(NEWLINE)
(DEDENT)
(KEYWORD if)
(ID "self")
(PUNCT ".")
(ID "dispatch")
(PUNCT "[")
(ID "event")
(PUNCT "]")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "frame")
(PUNCT ",")
(ID "t")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "t")
(PUNCT "=")
(ID "timer")
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
(ID "t")
(PUNCT "=")
(ID "timer")
(PUNCT "(")
(PUNCT ")")
(PUNCT "-")
(ID "t")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD def)
(ID "trace_dispatch_mac")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "frame")
(PUNCT ",")
(ID "event")
(PUNCT ",")
(ID "arg")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "timer")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "timer")
(NEWLINE)
(ID "t")
(PUNCT "=")
(ID "timer")
(PUNCT "(")
(PUNCT ")")
(PUNCT "/")
(LIT 60.0)
(PUNCT "-")
(ID "self")
(PUNCT ".")
(ID "t")
(PUNCT "-")
(ID "self")
(PUNCT ".")
(ID "bias")
(NEWLINE)
(KEYWORD if)
(ID "event")
(PUNCT "==")
(LIT "c_call")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "c_func_name")
(PUNCT "=")
(ID "arg")
(PUNCT ".")
(ID "__name__")
(NEWLINE)
(DEDENT)
(KEYWORD if)
(ID "self")
(PUNCT ".")
(ID "dispatch")
(PUNCT "[")
(ID "event")
(PUNCT "]")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "frame")
(PUNCT ",")
(ID "t")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "t")
(PUNCT "=")
(ID "timer")
(PUNCT "(")
(PUNCT ")")
(PUNCT "/")
(LIT 60.0)
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "t")
(PUNCT "=")
(ID "timer")
(PUNCT "(")
(PUNCT ")")
(PUNCT "/")
(LIT 60.0)
(PUNCT "-")
(ID "t")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD def)
(ID "trace_dispatch_l")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "frame")
(PUNCT ",")
(ID "event")
(PUNCT ",")
(ID "arg")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "get_time")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "get_time")
(NEWLINE)
(ID "t")
(PUNCT "=")
(ID "get_time")
(PUNCT "(")
(PUNCT ")")
(PUNCT "-")
(ID "self")
(PUNCT ".")
(ID "t")
(PUNCT "-")
(ID "self")
(PUNCT ".")
(ID "bias")
(NEWLINE)
(KEYWORD if)
(ID "event")
(PUNCT "==")
(LIT "c_call")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "c_func_name")
(PUNCT "=")
(ID "arg")
(PUNCT ".")
(ID "__name__")
(NEWLINE)
(DEDENT)
(KEYWORD if)
(ID "self")
(PUNCT ".")
(ID "dispatch")
(PUNCT "[")
(ID "event")
(PUNCT "]")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "frame")
(PUNCT ",")
(ID "t")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "t")
(PUNCT "=")
(ID "get_time")
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
(ID "t")
(PUNCT "=")
(ID "get_time")
(PUNCT "(")
(PUNCT ")")
(PUNCT "-")
(ID "t")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD def)
(ID "trace_dispatch_exception")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "frame")
(PUNCT ",")
(ID "t")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "rpt")
(PUNCT ",")
(ID "rit")
(PUNCT ",")
(ID "ret")
(PUNCT ",")
(ID "rfn")
(PUNCT ",")
(ID "rframe")
(PUNCT ",")
(ID "rcur")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "cur")
(NEWLINE)
(KEYWORD if)
(PUNCT "(")
(ID "rframe")
(KEYWORD is)
(KEYWORD not)
(ID "frame")
(PUNCT ")")
(KEYWORD and)
(ID "rcur")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(ID "self")
(PUNCT ".")
(ID "trace_dispatch_return")
(PUNCT "(")
(ID "rframe")
(PUNCT ",")
(ID "t")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "self")
(PUNCT ".")
(ID "cur")
(PUNCT "=")
(ID "rpt")
(PUNCT ",")
(ID "rit")
(PUNCT "+")
(ID "t")
(PUNCT ",")
(ID "ret")
(PUNCT ",")
(ID "rfn")
(PUNCT ",")
(ID "rframe")
(PUNCT ",")
(ID "rcur")
(NEWLINE)
(KEYWORD return)
(LIT 1)
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "trace_dispatch_call")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "frame")
(PUNCT ",")
(ID "t")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "self")
(PUNCT ".")
(ID "cur")
(KEYWORD and)
(ID "frame")
(PUNCT ".")
(ID "f_back")
(KEYWORD is)
(KEYWORD not)
(ID "self")
(PUNCT ".")
(ID "cur")
(PUNCT "[")
(PUNCT "-")
(LIT 2)
(PUNCT "]")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "rpt")
(PUNCT ",")
(ID "rit")
(PUNCT ",")
(ID "ret")
(PUNCT ",")
(ID "rfn")
(PUNCT ",")
(ID "rframe")
(PUNCT ",")
(ID "rcur")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "cur")
(NEWLINE)
(KEYWORD if)
(KEYWORD not)
(ID "isinstance")
(PUNCT "(")
(ID "rframe")
(PUNCT ",")
(ID "Profile")
(PUNCT ".")
(ID "fake_frame")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD assert)
(ID "rframe")
(PUNCT ".")
(ID "f_back")
(KEYWORD is)
(ID "frame")
(PUNCT ".")
(ID "f_back")
(PUNCT ",")
(PUNCT "(")
(LIT "Bad call")
(PUNCT ",")
(ID "rfn")
(PUNCT ",")
(ID "rframe")
(PUNCT ",")
(ID "rframe")
(PUNCT ".")
(ID "f_back")
(PUNCT ",")
(ID "frame")
(PUNCT ",")
(ID "frame")
(PUNCT ".")
(ID "f_back")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "trace_dispatch_return")
(PUNCT "(")
(ID "rframe")
(PUNCT ",")
(LIT 0)
(PUNCT ")")
(NEWLINE)
(KEYWORD assert)
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "cur")
(KEYWORD is)
(KEYWORD None)
(KEYWORD or)
(ID "frame")
(PUNCT ".")
(ID "f_back")
(KEYWORD is)
(ID "self")
(PUNCT ".")
(ID "cur")
(PUNCT "[")
(PUNCT "-")
(LIT 2)
(PUNCT "]")
(PUNCT ")")
(PUNCT ",")
(PUNCT "(")
(LIT "Bad call")
(PUNCT ",")
(ID "self")
(PUNCT ".")
(ID "cur")
(PUNCT "[")
(PUNCT "-")
(LIT 3)
(PUNCT "]")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(ID "fcode")
(PUNCT "=")
(ID "frame")
(PUNCT ".")
(ID "f_code")
(NEWLINE)
(ID "fn")
(PUNCT "=")
(PUNCT "(")
(ID "fcode")
(PUNCT ".")
(ID "co_filename")
(PUNCT ",")
(ID "fcode")
(PUNCT ".")
(ID "co_firstlineno")
(PUNCT ",")
(ID "fcode")
(PUNCT ".")
(ID "co_name")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "cur")
(PUNCT "=")
(PUNCT "(")
(ID "t")
(PUNCT ",")
(LIT 0)
(PUNCT ",")
(LIT 0)
(PUNCT ",")
(ID "fn")
(PUNCT ",")
(ID "frame")
(PUNCT ",")
(ID "self")
(PUNCT ".")
(ID "cur")
(PUNCT ")")
(NEWLINE)
(ID "timings")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "timings")
(NEWLINE)
(KEYWORD if)
(ID "fn")
(KEYWORD in)
(ID "timings")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "cc")
(PUNCT ",")
(ID "ns")
(PUNCT ",")
(ID "tt")
(PUNCT ",")
(ID "ct")
(PUNCT ",")
(ID "callers")
(PUNCT "=")
(ID "timings")
(PUNCT "[")
(ID "fn")
(PUNCT "]")
(NEWLINE)
(ID "timings")
(PUNCT "[")
(ID "fn")
(PUNCT "]")
(PUNCT "=")
(ID "cc")
(PUNCT ",")
(ID "ns")
(PUNCT "+")
(LIT 1)
(PUNCT ",")
(ID "tt")
(PUNCT ",")
(ID "ct")
(PUNCT ",")
(ID "callers")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "timings")
(PUNCT "[")
(ID "fn")
(PUNCT "]")
(PUNCT "=")
(LIT 0)
(PUNCT ",")
(LIT 0)
(PUNCT ",")
(LIT 0)
(PUNCT ",")
(LIT 0)
(PUNCT ",")
(PUNCT "{")
(PUNCT "}")
(NEWLINE)
(DEDENT)
(KEYWORD return)
(LIT 1)
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "trace_dispatch_c_call")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "frame")
(PUNCT ",")
(ID "t")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "fn")
(PUNCT "=")
(PUNCT "(")
(LIT "")
(PUNCT ",")
(LIT 0)
(PUNCT ",")
(ID "self")
(PUNCT ".")
(ID "c_func_name")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "cur")
(PUNCT "=")
(PUNCT "(")
(ID "t")
(PUNCT ",")
(LIT 0)
(PUNCT ",")
(LIT 0)
(PUNCT ",")
(ID "fn")
(PUNCT ",")
(ID "frame")
(PUNCT ",")
(ID "self")
(PUNCT ".")
(ID "cur")
(PUNCT ")")
(NEWLINE)
(ID "timings")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "timings")
(NEWLINE)
(KEYWORD if)
(ID "fn")
(KEYWORD in)
(ID "timings")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "cc")
(PUNCT ",")
(ID "ns")
(PUNCT ",")
(ID "tt")
(PUNCT ",")
(ID "ct")
(PUNCT ",")
(ID "callers")
(PUNCT "=")
(ID "timings")
(PUNCT "[")
(ID "fn")
(PUNCT "]")
(NEWLINE)
(ID "timings")
(PUNCT "[")
(ID "fn")
(PUNCT "]")
(PUNCT "=")
(ID "cc")
(PUNCT ",")
(ID "ns")
(PUNCT "+")
(LIT 1)
(PUNCT ",")
(ID "tt")
(PUNCT ",")
(ID "ct")
(PUNCT ",")
(ID "callers")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "timings")
(PUNCT "[")
(ID "fn")
(PUNCT "]")
(PUNCT "=")
(LIT 0)
(PUNCT ",")
(LIT 0)
(PUNCT ",")
(LIT 0)
(PUNCT ",")
(LIT 0)
(PUNCT ",")
(PUNCT "{")
(PUNCT "}")
(NEWLINE)
(DEDENT)
(KEYWORD return)
(LIT 1)
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "trace_dispatch_return")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "frame")
(PUNCT ",")
(ID "t")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "frame")
(KEYWORD is)
(KEYWORD not)
(ID "self")
(PUNCT ".")
(ID "cur")
(PUNCT "[")
(PUNCT "-")
(LIT 2)
(PUNCT "]")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD assert)
(ID "frame")
(KEYWORD is)
(ID "self")
(PUNCT ".")
(ID "cur")
(PUNCT "[")
(PUNCT "-")
(LIT 2)
(PUNCT "]")
(PUNCT ".")
(ID "f_back")
(PUNCT ",")
(PUNCT "(")
(LIT "Bad return")
(PUNCT ",")
(ID "self")
(PUNCT ".")
(ID "cur")
(PUNCT "[")
(PUNCT "-")
(LIT 3)
(PUNCT "]")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "trace_dispatch_return")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "cur")
(PUNCT "[")
(PUNCT "-")
(LIT 2)
(PUNCT "]")
(PUNCT ",")
(LIT 0)
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "rpt")
(PUNCT ",")
(ID "rit")
(PUNCT ",")
(ID "ret")
(PUNCT ",")
(ID "rfn")
(PUNCT ",")
(ID "frame")
(PUNCT ",")
(ID "rcur")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "cur")
(NEWLINE)
(ID "rit")
(PUNCT "=")
(ID "rit")
(PUNCT "+")
(ID "t")
(NEWLINE)
(ID "frame_total")
(PUNCT "=")
(ID "rit")
(PUNCT "+")
(ID "ret")
(NEWLINE)
(ID "ppt")
(PUNCT ",")
(ID "pit")
(PUNCT ",")
(ID "pet")
(PUNCT ",")
(ID "pfn")
(PUNCT ",")
(ID "pframe")
(PUNCT ",")
(ID "pcur")
(PUNCT "=")
(ID "rcur")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "cur")
(PUNCT "=")
(ID "ppt")
(PUNCT ",")
(ID "pit")
(PUNCT "+")
(ID "rpt")
(PUNCT ",")
(ID "pet")
(PUNCT "+")
(ID "frame_total")
(PUNCT ",")
(ID "pfn")
(PUNCT ",")
(ID "pframe")
(PUNCT ",")
(ID "pcur")
(NEWLINE)
(ID "timings")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "timings")
(NEWLINE)
(ID "cc")
(PUNCT ",")
(ID "ns")
(PUNCT ",")
(ID "tt")
(PUNCT ",")
(ID "ct")
(PUNCT ",")
(ID "callers")
(PUNCT "=")
(ID "timings")
(PUNCT "[")
(ID "rfn")
(PUNCT "]")
(NEWLINE)
(KEYWORD if)
(KEYWORD not)
(ID "ns")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "ct")
(PUNCT "=")
(ID "ct")
(PUNCT "+")
(ID "frame_total")
(NEWLINE)
(ID "cc")
(PUNCT "=")
(ID "cc")
(PUNCT "+")
(LIT 1)
(NEWLINE)
(DEDENT)
(KEYWORD if)
(ID "pfn")
(KEYWORD in)
(ID "callers")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "callers")
(PUNCT "[")
(ID "pfn")
(PUNCT "]")
(PUNCT "=")
(ID "callers")
(PUNCT "[")
(ID "pfn")
(PUNCT "]")
(PUNCT "+")
(LIT 1)
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "callers")
(PUNCT "[")
(ID "pfn")
(PUNCT "]")
(PUNCT "=")
(LIT 1)
(NEWLINE)
(DEDENT)
(ID "timings")
(PUNCT "[")
(ID "rfn")
(PUNCT "]")
(PUNCT "=")
(ID "cc")
(PUNCT ",")
(ID "ns")
(PUNCT "-")
(LIT 1)
(PUNCT ",")
(ID "tt")
(PUNCT "+")
(ID "rit")
(PUNCT ",")
(ID "ct")
(PUNCT ",")
(ID "callers")
(NEWLINE)
(KEYWORD return)
(LIT 1)
(NEWLINE)
(DEDENT)
(ID "dispatch")
(PUNCT "=")
(PUNCT "{")
(LIT "call")
(PUNCT ":")
(ID "trace_dispatch_call")
(PUNCT ",")
(LIT "exception")
(PUNCT ":")
(ID "trace_dispatch_exception")
(PUNCT ",")
(LIT "return")
(PUNCT ":")
(ID "trace_dispatch_return")
(PUNCT ",")
(LIT "c_call")
(PUNCT ":")
(ID "trace_dispatch_c_call")
(PUNCT ",")
(LIT "c_exception")
(PUNCT ":")
(ID "trace_dispatch_return")
(PUNCT ",")
(LIT "c_return")
(PUNCT ":")
(ID "trace_dispatch_return")
(PUNCT ",")
(PUNCT "}")
(NEWLINE)
(KEYWORD def)
(ID "set_cmd")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "cmd")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "self")
(PUNCT ".")
(ID "cur")
(PUNCT "[")
(PUNCT "-")
(LIT 1)
(PUNCT "]")
(PUNCT ":")
(KEYWORD return)
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "cmd")
(PUNCT "=")
(ID "cmd")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "simulate_call")
(PUNCT "(")
(ID "cmd")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD class)
(ID "fake_code")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD def)
(ID "__init__")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "filename")
(PUNCT ",")
(ID "line")
(PUNCT ",")
(ID "name")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "co_filename")
(PUNCT "=")
(ID "filename")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "co_line")
(PUNCT "=")
(ID "line")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "co_name")
(PUNCT "=")
(ID "name")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "co_firstlineno")
(PUNCT "=")
(LIT 0)
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
(ID "repr")
(PUNCT "(")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "co_filename")
(PUNCT ",")
(ID "self")
(PUNCT ".")
(ID "co_line")
(PUNCT ",")
(ID "self")
(PUNCT ".")
(ID "co_name")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD class)
(ID "fake_frame")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD def)
(ID "__init__")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "code")
(PUNCT ",")
(ID "prior")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "f_code")
(PUNCT "=")
(ID "code")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "f_back")
(PUNCT "=")
(ID "prior")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD def)
(ID "simulate_call")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "name")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "code")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "fake_code")
(PUNCT "(")
(LIT "profile")
(PUNCT ",")
(LIT 0)
(PUNCT ",")
(ID "name")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(ID "self")
(PUNCT ".")
(ID "cur")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "pframe")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "cur")
(PUNCT "[")
(PUNCT "-")
(LIT 2)
(PUNCT "]")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "pframe")
(PUNCT "=")
(KEYWORD None)
(NEWLINE)
(DEDENT)
(ID "frame")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "fake_frame")
(PUNCT "(")
(ID "code")
(PUNCT ",")
(ID "pframe")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "dispatch")
(PUNCT "[")
(LIT "call")
(PUNCT "]")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "frame")
(PUNCT ",")
(LIT 0)
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "simulate_cmd_complete")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "get_time")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "get_time")
(NEWLINE)
(ID "t")
(PUNCT "=")
(ID "get_time")
(PUNCT "(")
(PUNCT ")")
(PUNCT "-")
(ID "self")
(PUNCT ".")
(ID "t")
(NEWLINE)
(KEYWORD while)
(ID "self")
(PUNCT ".")
(ID "cur")
(PUNCT "[")
(PUNCT "-")
(LIT 1)
(PUNCT "]")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "dispatch")
(PUNCT "[")
(LIT "return")
(PUNCT "]")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "self")
(PUNCT ".")
(ID "cur")
(PUNCT "[")
(PUNCT "-")
(LIT 2)
(PUNCT "]")
(PUNCT ",")
(ID "t")
(PUNCT ")")
(NEWLINE)
(ID "t")
(PUNCT "=")
(LIT 0)
(NEWLINE)
(DEDENT)
(ID "self")
(PUNCT ".")
(ID "t")
(PUNCT "=")
(ID "get_time")
(PUNCT "(")
(PUNCT ")")
(PUNCT "-")
(ID "t")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "print_stats")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "sort")
(PUNCT "=")
(PUNCT "-")
(LIT 1)
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD import)
(ID "pstats")
(NEWLINE)
(ID "pstats")
(PUNCT ".")
(ID "Stats")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ".")
(ID "strip_dirs")
(PUNCT "(")
(PUNCT ")")
(PUNCT ".")
(ID "sort_stats")
(PUNCT "(")
(ID "sort")
(PUNCT ")")
(PUNCT ".")
(ID "print_stats")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "dump_stats")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "file")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD with)
(ID "open")
(PUNCT "(")
(ID "file")
(PUNCT ",")
(LIT "wb")
(PUNCT ")")
(KEYWORD as)
(ID "f")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "create_stats")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "marshal")
(PUNCT ".")
(ID "dump")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "stats")
(PUNCT ",")
(ID "f")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD def)
(ID "create_stats")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "simulate_cmd_complete")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "snapshot_stats")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "snapshot_stats")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "stats")
(PUNCT "=")
(PUNCT "{")
(PUNCT "}")
(NEWLINE)
(KEYWORD for)
(ID "func")
(PUNCT ",")
(PUNCT "(")
(ID "cc")
(PUNCT ",")
(ID "ns")
(PUNCT ",")
(ID "tt")
(PUNCT ",")
(ID "ct")
(PUNCT ",")
(ID "callers")
(PUNCT ")")
(KEYWORD in)
(ID "self")
(PUNCT ".")
(ID "timings")
(PUNCT ".")
(ID "items")
(PUNCT "(")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "callers")
(PUNCT "=")
(ID "callers")
(PUNCT ".")
(ID "copy")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "nc")
(PUNCT "=")
(LIT 0)
(NEWLINE)
(KEYWORD for)
(ID "callcnt")
(KEYWORD in)
(ID "callers")
(PUNCT ".")
(ID "values")
(PUNCT "(")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "nc")
(PUNCT "+=")
(ID "callcnt")
(NEWLINE)
(DEDENT)
(ID "self")
(PUNCT ".")
(ID "stats")
(PUNCT "[")
(ID "func")
(PUNCT "]")
(PUNCT "=")
(ID "cc")
(PUNCT ",")
(ID "nc")
(PUNCT ",")
(ID "tt")
(PUNCT ",")
(ID "ct")
(PUNCT ",")
(ID "callers")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD def)
(ID "run")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "cmd")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD import)
(ID "__main__")
(NEWLINE)
(ID "dict")
(PUNCT "=")
(ID "__main__")
(PUNCT ".")
(ID "__dict__")
(NEWLINE)
(KEYWORD return)
(ID "self")
(PUNCT ".")
(ID "runctx")
(PUNCT "(")
(ID "cmd")
(PUNCT ",")
(ID "dict")
(PUNCT ",")
(ID "dict")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "runctx")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "cmd")
(PUNCT ",")
(ID "globals")
(PUNCT ",")
(ID "locals")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "set_cmd")
(PUNCT "(")
(ID "cmd")
(PUNCT ")")
(NEWLINE)
(ID "sys")
(PUNCT ".")
(ID "setprofile")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "dispatcher")
(PUNCT ")")
(NEWLINE)
(KEYWORD try)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "exec")
(PUNCT "(")
(ID "cmd")
(PUNCT ",")
(ID "globals")
(PUNCT ",")
(ID "locals")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD finally)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "sys")
(PUNCT ".")
(ID "setprofile")
(PUNCT "(")
(KEYWORD None)
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD return)
(ID "self")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "runcall")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "func")
(PUNCT ",")
(PUNCT "*")
(ID "args")
(PUNCT ",")
(PUNCT "**")
(ID "kw")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "set_cmd")
(PUNCT "(")
(ID "repr")
(PUNCT "(")
(ID "func")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(ID "sys")
(PUNCT ".")
(ID "setprofile")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "dispatcher")
(PUNCT ")")
(NEWLINE)
(KEYWORD try)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(ID "func")
(PUNCT "(")
(PUNCT "*")
(ID "args")
(PUNCT ",")
(PUNCT "**")
(ID "kw")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD finally)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "sys")
(PUNCT ".")
(ID "setprofile")
(PUNCT "(")
(KEYWORD None)
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD def)
(ID "calibrate")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "m")
(PUNCT ",")
(ID "verbose")
(PUNCT "=")
(LIT 0)
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "self")
(PUNCT ".")
(ID "__class__")
(KEYWORD is)
(KEYWORD not)
(ID "Profile")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD raise)
(ID "TypeError")
(PUNCT "(")
(LIT "Subclasses must override .calibrate().")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "saved_bias")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "bias")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "bias")
(PUNCT "=")
(LIT 0)
(NEWLINE)
(KEYWORD try)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(ID "self")
(PUNCT ".")
(ID "_calibrate_inner")
(PUNCT "(")
(ID "m")
(PUNCT ",")
(ID "verbose")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD finally)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "bias")
(PUNCT "=")
(ID "saved_bias")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD def)
(ID "_calibrate_inner")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "m")
(PUNCT ",")
(ID "verbose")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "get_time")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "get_time")
(NEWLINE)
(KEYWORD def)
(ID "f1")
(PUNCT "(")
(ID "n")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD for)
(ID "i")
(KEYWORD in)
(ID "range")
(PUNCT "(")
(ID "n")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "x")
(PUNCT "=")
(LIT 1)
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD def)
(ID "f")
(PUNCT "(")
(ID "m")
(PUNCT ",")
(ID "f1")
(PUNCT "=")
(ID "f1")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD for)
(ID "i")
(KEYWORD in)
(ID "range")
(PUNCT "(")
(ID "m")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "f1")
(PUNCT "(")
(LIT 100)
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(ID "f")
(PUNCT "(")
(ID "m")
(PUNCT ")")
(NEWLINE)
(ID "t0")
(PUNCT "=")
(ID "get_time")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "f")
(PUNCT "(")
(ID "m")
(PUNCT ")")
(NEWLINE)
(ID "t1")
(PUNCT "=")
(ID "get_time")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "elapsed_noprofile")
(PUNCT "=")
(ID "t1")
(PUNCT "-")
(ID "t0")
(NEWLINE)
(KEYWORD if)
(ID "verbose")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "print")
(PUNCT "(")
(LIT "elapsed time without profiling =")
(PUNCT ",")
(ID "elapsed_noprofile")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "p")
(PUNCT "=")
(ID "Profile")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "t0")
(PUNCT "=")
(ID "get_time")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "p")
(PUNCT ".")
(ID "runctx")
(PUNCT "(")
(LIT "f(m)")
(PUNCT ",")
(ID "globals")
(PUNCT "(")
(PUNCT ")")
(PUNCT ",")
(ID "locals")
(PUNCT "(")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(ID "t1")
(PUNCT "=")
(ID "get_time")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "elapsed_profile")
(PUNCT "=")
(ID "t1")
(PUNCT "-")
(ID "t0")
(NEWLINE)
(KEYWORD if)
(ID "verbose")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "print")
(PUNCT "(")
(LIT "elapsed time with profiling =")
(PUNCT ",")
(ID "elapsed_profile")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "total_calls")
(PUNCT "=")
(LIT 0.0)
(NEWLINE)
(ID "reported_time")
(PUNCT "=")
(LIT 0.0)
(NEWLINE)
(KEYWORD for)
(PUNCT "(")
(ID "filename")
(PUNCT ",")
(ID "line")
(PUNCT ",")
(ID "funcname")
(PUNCT ")")
(PUNCT ",")
(PUNCT "(")
(ID "cc")
(PUNCT ",")
(ID "ns")
(PUNCT ",")
(ID "tt")
(PUNCT ",")
(ID "ct")
(PUNCT ",")
(ID "callers")
(PUNCT ")")
(KEYWORD in)
(ID "p")
(PUNCT ".")
(ID "timings")
(PUNCT ".")
(ID "items")
(PUNCT "(")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "funcname")
(KEYWORD in)
(PUNCT "(")
(LIT "f")
(PUNCT ",")
(LIT "f1")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "total_calls")
(PUNCT "+=")
(ID "cc")
(NEWLINE)
(ID "reported_time")
(PUNCT "+=")
(ID "tt")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD if)
(ID "verbose")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "print")
(PUNCT "(")
(LIT "'CPU seconds' profiler reported =")
(PUNCT ",")
(ID "reported_time")
(PUNCT ")")
(NEWLINE)
(ID "print")
(PUNCT "(")
(LIT "total # calls =")
(PUNCT ",")
(ID "total_calls")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD if)
(ID "total_calls")
(PUNCT "!=")
(ID "m")
(PUNCT "+")
(LIT 1)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD raise)
(ID "ValueError")
(PUNCT "(")
(LIT "internal error: total calls = %d")
(PUNCT "%")
(ID "total_calls")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "mean")
(PUNCT "=")
(PUNCT "(")
(ID "reported_time")
(PUNCT "-")
(ID "elapsed_noprofile")
(PUNCT ")")
(PUNCT "/")
(LIT 2.0)
(PUNCT "/")
(ID "total_calls")
(NEWLINE)
(KEYWORD if)
(ID "verbose")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "print")
(PUNCT "(")
(LIT "mean stopwatch overhead per profile event =")
(PUNCT ",")
(ID "mean")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD return)
(ID "mean")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD def)
(ID "main")
(PUNCT "(")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "usage")
(PUNCT "=")
(LIT "profile.py [-o output_file_path] [-s sort] scriptfile [arg] ...")
(NEWLINE)
(ID "parser")
(PUNCT "=")
(ID "OptionParser")
(PUNCT "(")
(ID "usage")
(PUNCT "=")
(ID "usage")
(PUNCT ")")
(NEWLINE)
(ID "parser")
(PUNCT ".")
(ID "allow_interspersed_args")
(PUNCT "=")
(KEYWORD False)
(NEWLINE)
(ID "parser")
(PUNCT ".")
(ID "add_option")
(PUNCT "(")
(LIT "-o")
(PUNCT ",")
(LIT "--outfile")
(PUNCT ",")
(ID "dest")
(PUNCT "=")
(LIT "outfile")
(PUNCT ",")
(ID "help")
(PUNCT "=")
(LIT "Save stats to <outfile>")
(PUNCT ",")
(ID "default")
(PUNCT "=")
(KEYWORD None)
(PUNCT ")")
(NEWLINE)
(ID "parser")
(PUNCT ".")
(ID "add_option")
(PUNCT "(")
(LIT "-s")
(PUNCT ",")
(LIT "--sort")
(PUNCT ",")
(ID "dest")
(PUNCT "=")
(LIT "sort")
(PUNCT ",")
(ID "help")
(PUNCT "=")
(LIT "Sort order when printing to stdout, based on pstats.Stats class")
(PUNCT ",")
(ID "default")
(PUNCT "=")
(PUNCT "-")
(LIT 1)
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(KEYWORD not)
(ID "sys")
(PUNCT ".")
(ID "argv")
(PUNCT "[")
(LIT 1)
(PUNCT ":")
(PUNCT "]")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "parser")
(PUNCT ".")
(ID "print_usage")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "sys")
(PUNCT ".")
(ID "exit")
(PUNCT "(")
(LIT 2)
(PUNCT ")")
(NEWLINE)
(DEDENT)
(PUNCT "(")
(ID "options")
(PUNCT ",")
(ID "args")
(PUNCT ")")
(PUNCT "=")
(ID "parser")
(PUNCT ".")
(ID "parse_args")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "sys")
(PUNCT ".")
(ID "argv")
(PUNCT "[")
(PUNCT ":")
(PUNCT "]")
(PUNCT "=")
(ID "args")
(NEWLINE)
(KEYWORD if)
(ID "len")
(PUNCT "(")
(ID "args")
(PUNCT ")")
(PUNCT ">")
(LIT 0)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "progname")
(PUNCT "=")
(ID "args")
(PUNCT "[")
(LIT 0)
(PUNCT "]")
(NEWLINE)
(ID "sys")
(PUNCT ".")
(ID "path")
(PUNCT ".")
(ID "insert")
(PUNCT "(")
(LIT 0)
(PUNCT ",")
(ID "os")
(PUNCT ".")
(ID "path")
(PUNCT ".")
(ID "dirname")
(PUNCT "(")
(ID "progname")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(KEYWORD with)
(ID "open")
(PUNCT "(")
(ID "progname")
(PUNCT ",")
(LIT "rb")
(PUNCT ")")
(KEYWORD as)
(ID "fp")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "code")
(PUNCT "=")
(ID "compile")
(PUNCT "(")
(ID "fp")
(PUNCT ".")
(ID "read")
(PUNCT "(")
(PUNCT ")")
(PUNCT ",")
(ID "progname")
(PUNCT ",")
(LIT "exec")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "globs")
(PUNCT "=")
(PUNCT "{")
(LIT "__file__")
(PUNCT ":")
(ID "progname")
(PUNCT ",")
(LIT "__name__")
(PUNCT ":")
(LIT "__main__")
(PUNCT ",")
(LIT "__package__")
(PUNCT ":")
(KEYWORD None)
(PUNCT ",")
(LIT "__cached__")
(PUNCT ":")
(KEYWORD None)
(PUNCT ",")
(PUNCT "}")
(NEWLINE)
(ID "runctx")
(PUNCT "(")
(ID "code")
(PUNCT ",")
(ID "globs")
(PUNCT ",")
(KEYWORD None)
(PUNCT ",")
(ID "options")
(PUNCT ".")
(ID "outfile")
(PUNCT ",")
(ID "options")
(PUNCT ".")
(ID "sort")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "parser")
(PUNCT ".")
(ID "print_usage")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD return)
(ID "parser")
(NEWLINE)
(DEDENT)
(KEYWORD if)
(ID "__name__")
(PUNCT "==")
(LIT "__main__")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "main")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ENDMARKER)
