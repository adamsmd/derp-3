(KEYWORD import)
(ID "os")
(NEWLINE)
(KEYWORD import)
(ID "fnmatch")
(NEWLINE)
(KEYWORD import)
(ID "re")
(NEWLINE)
(KEYWORD import)
(ID "sys")
(NEWLINE)
(KEYWORD from)
(ID "tkinter")
(KEYWORD import)
(ID "StringVar")
(PUNCT ",")
(ID "BooleanVar")
(PUNCT ",")
(ID "Checkbutton")
(NEWLINE)
(KEYWORD from)
(ID "tkinter")
(KEYWORD import)
(ID "Tk")
(PUNCT ",")
(ID "Text")
(PUNCT ",")
(ID "Button")
(PUNCT ",")
(ID "SEL")
(PUNCT ",")
(ID "END")
(NEWLINE)
(KEYWORD from)
(ID "idlelib")
(KEYWORD import)
(ID "SearchEngine")
(NEWLINE)
(KEYWORD import)
(ID "itertools")
(NEWLINE)
(KEYWORD from)
(ID "idlelib")
(PUNCT ".")
(ID "SearchDialogBase")
(KEYWORD import)
(ID "SearchDialogBase")
(NEWLINE)
(KEYWORD def)
(ID "grep")
(PUNCT "(")
(ID "text")
(PUNCT ",")
(ID "io")
(PUNCT "=")
(KEYWORD None)
(PUNCT ",")
(ID "flist")
(PUNCT "=")
(KEYWORD None)
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "root")
(PUNCT "=")
(ID "text")
(PUNCT ".")
(ID "_root")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "engine")
(PUNCT "=")
(ID "SearchEngine")
(PUNCT ".")
(ID "get")
(PUNCT "(")
(ID "root")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(KEYWORD not)
(ID "hasattr")
(PUNCT "(")
(ID "engine")
(PUNCT ",")
(LIT "_grepdialog")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "engine")
(PUNCT ".")
(ID "_grepdialog")
(PUNCT "=")
(ID "GrepDialog")
(PUNCT "(")
(ID "root")
(PUNCT ",")
(ID "engine")
(PUNCT ",")
(ID "flist")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "dialog")
(PUNCT "=")
(ID "engine")
(PUNCT ".")
(ID "_grepdialog")
(NEWLINE)
(ID "searchphrase")
(PUNCT "=")
(ID "text")
(PUNCT ".")
(ID "get")
(PUNCT "(")
(LIT "sel.first")
(PUNCT ",")
(LIT "sel.last")
(PUNCT ")")
(NEWLINE)
(ID "dialog")
(PUNCT ".")
(ID "open")
(PUNCT "(")
(ID "text")
(PUNCT ",")
(ID "searchphrase")
(PUNCT ",")
(ID "io")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD class)
(ID "GrepDialog")
(PUNCT "(")
(ID "SearchDialogBase")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "title")
(PUNCT "=")
(LIT "Find in Files Dialog")
(NEWLINE)
(ID "icon")
(PUNCT "=")
(LIT "Grep")
(NEWLINE)
(ID "needwrapbutton")
(PUNCT "=")
(LIT 0)
(NEWLINE)
(KEYWORD def)
(ID "__init__")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "root")
(PUNCT ",")
(ID "engine")
(PUNCT ",")
(ID "flist")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "SearchDialogBase")
(PUNCT ".")
(ID "__init__")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "root")
(PUNCT ",")
(ID "engine")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "flist")
(PUNCT "=")
(ID "flist")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "globvar")
(PUNCT "=")
(ID "StringVar")
(PUNCT "(")
(ID "root")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "recvar")
(PUNCT "=")
(ID "BooleanVar")
(PUNCT "(")
(ID "root")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "open")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "text")
(PUNCT ",")
(ID "searchphrase")
(PUNCT ",")
(ID "io")
(PUNCT "=")
(KEYWORD None)
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "SearchDialogBase")
(PUNCT ".")
(ID "open")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "text")
(PUNCT ",")
(ID "searchphrase")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(ID "io")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "path")
(PUNCT "=")
(ID "io")
(PUNCT ".")
(ID "filename")
(KEYWORD or)
(LIT "")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "path")
(PUNCT "=")
(LIT "")
(NEWLINE)
(DEDENT)
(ID "dir")
(PUNCT ",")
(ID "base")
(PUNCT "=")
(ID "os")
(PUNCT ".")
(ID "path")
(PUNCT ".")
(ID "split")
(PUNCT "(")
(ID "path")
(PUNCT ")")
(NEWLINE)
(ID "head")
(PUNCT ",")
(ID "tail")
(PUNCT "=")
(ID "os")
(PUNCT ".")
(ID "path")
(PUNCT ".")
(ID "splitext")
(PUNCT "(")
(ID "base")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(KEYWORD not)
(ID "tail")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "tail")
(PUNCT "=")
(LIT ".py")
(NEWLINE)
(DEDENT)
(ID "self")
(PUNCT ".")
(ID "globvar")
(PUNCT ".")
(ID "set")
(PUNCT "(")
(ID "os")
(PUNCT ".")
(ID "path")
(PUNCT ".")
(ID "join")
(PUNCT "(")
(ID "dir")
(PUNCT ",")
(LIT "*")
(PUNCT "+")
(ID "tail")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "create_entries")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "SearchDialogBase")
(PUNCT ".")
(ID "create_entries")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "globent")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "make_entry")
(PUNCT "(")
(LIT "In files:")
(PUNCT ",")
(ID "self")
(PUNCT ".")
(ID "globvar")
(PUNCT ")")
(PUNCT "[")
(LIT 0)
(PUNCT "]")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "create_other_buttons")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "f")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "make_frame")
(PUNCT "(")
(PUNCT ")")
(PUNCT "[")
(LIT 0)
(PUNCT "]")
(NEWLINE)
(ID "btn")
(PUNCT "=")
(ID "Checkbutton")
(PUNCT "(")
(ID "f")
(PUNCT ",")
(ID "anchor")
(PUNCT "=")
(LIT "w")
(PUNCT ",")
(ID "variable")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "recvar")
(PUNCT ",")
(ID "text")
(PUNCT "=")
(LIT "Recurse down subdirectories")
(PUNCT ")")
(NEWLINE)
(ID "btn")
(PUNCT ".")
(ID "pack")
(PUNCT "(")
(ID "side")
(PUNCT "=")
(LIT "top")
(PUNCT ",")
(ID "fill")
(PUNCT "=")
(LIT "both")
(PUNCT ")")
(NEWLINE)
(ID "btn")
(PUNCT ".")
(ID "select")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "create_command_buttons")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "SearchDialogBase")
(PUNCT ".")
(ID "create_command_buttons")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "make_button")
(PUNCT "(")
(LIT "Search Files")
(PUNCT ",")
(ID "self")
(PUNCT ".")
(ID "default_command")
(PUNCT ",")
(LIT 1)
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "default_command")
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
(ID "prog")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "engine")
(PUNCT ".")
(ID "getprog")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(KEYWORD not)
(ID "prog")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(NEWLINE)
(DEDENT)
(ID "path")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "globvar")
(PUNCT ".")
(ID "get")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(KEYWORD not)
(ID "path")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "top")
(PUNCT ".")
(ID "bell")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(KEYWORD return)
(NEWLINE)
(DEDENT)
(KEYWORD from)
(ID "idlelib")
(PUNCT ".")
(ID "OutputWindow")
(KEYWORD import)
(ID "OutputWindow")
(NEWLINE)
(ID "save")
(PUNCT "=")
(ID "sys")
(PUNCT ".")
(ID "stdout")
(NEWLINE)
(KEYWORD try)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "sys")
(PUNCT ".")
(ID "stdout")
(PUNCT "=")
(ID "OutputWindow")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "flist")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "grep_it")
(PUNCT "(")
(ID "prog")
(PUNCT ",")
(ID "path")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD finally)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "sys")
(PUNCT ".")
(ID "stdout")
(PUNCT "=")
(ID "save")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD def)
(ID "grep_it")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "prog")
(PUNCT ",")
(ID "path")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "dir")
(PUNCT ",")
(ID "base")
(PUNCT "=")
(ID "os")
(PUNCT ".")
(ID "path")
(PUNCT ".")
(ID "split")
(PUNCT "(")
(ID "path")
(PUNCT ")")
(NEWLINE)
(ID "list")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "findfiles")
(PUNCT "(")
(ID "dir")
(PUNCT ",")
(ID "base")
(PUNCT ",")
(ID "self")
(PUNCT ".")
(ID "recvar")
(PUNCT ".")
(ID "get")
(PUNCT "(")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(ID "list")
(PUNCT ".")
(ID "sort")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "close")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "pat")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "engine")
(PUNCT ".")
(ID "getpat")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "print")
(PUNCT "(")
(LIT "Searching %r in %s ...")
(PUNCT "%")
(PUNCT "(")
(ID "pat")
(PUNCT ",")
(ID "path")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(ID "hits")
(PUNCT "=")
(LIT 0)
(NEWLINE)
(KEYWORD try)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD for)
(ID "fn")
(KEYWORD in)
(ID "list")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD try)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD with)
(ID "open")
(PUNCT "(")
(ID "fn")
(PUNCT ",")
(ID "errors")
(PUNCT "=")
(LIT "replace")
(PUNCT ")")
(KEYWORD as)
(ID "f")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD for)
(ID "lineno")
(PUNCT ",")
(ID "line")
(KEYWORD in)
(ID "enumerate")
(PUNCT "(")
(ID "f")
(PUNCT ",")
(LIT 1)
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "line")
(PUNCT "[")
(PUNCT "-")
(LIT 1)
(PUNCT ":")
(PUNCT "]")
(PUNCT "==")
(LIT "\n")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "line")
(PUNCT "=")
(ID "line")
(PUNCT "[")
(PUNCT ":")
(PUNCT "-")
(LIT 1)
(PUNCT "]")
(NEWLINE)
(DEDENT)
(KEYWORD if)
(ID "prog")
(PUNCT ".")
(ID "search")
(PUNCT "(")
(ID "line")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "sys")
(PUNCT ".")
(ID "stdout")
(PUNCT ".")
(ID "write")
(PUNCT "(")
(LIT "%s: %s: %s\n")
(PUNCT "%")
(PUNCT "(")
(ID "fn")
(PUNCT ",")
(ID "lineno")
(PUNCT ",")
(ID "line")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(ID "hits")
(PUNCT "+=")
(LIT 1)
(NEWLINE)
(DEDENT)
(DEDENT)
(DEDENT)
(DEDENT)
(KEYWORD except)
(ID "OSError")
(KEYWORD as)
(ID "msg")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "print")
(PUNCT "(")
(ID "msg")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(ID "print")
(PUNCT "(")
(PUNCT "(")
(LIT "Hits found: %s\n")
(LIT "(Hint: right-click to open locations.)")
(PUNCT "%")
(ID "hits")
(PUNCT ")")
(KEYWORD if)
(ID "hits")
(KEYWORD else)
(LIT "No hits.")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD except)
(ID "AttributeError")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD pass)
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD def)
(ID "findfiles")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "dir")
(PUNCT ",")
(ID "base")
(PUNCT ",")
(ID "rec")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD try)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "names")
(PUNCT "=")
(ID "os")
(PUNCT ".")
(ID "listdir")
(PUNCT "(")
(ID "dir")
(KEYWORD or)
(ID "os")
(PUNCT ".")
(ID "curdir")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD except)
(ID "OSError")
(KEYWORD as)
(ID "msg")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "print")
(PUNCT "(")
(ID "msg")
(PUNCT ")")
(NEWLINE)
(KEYWORD return)
(PUNCT "[")
(PUNCT "]")
(NEWLINE)
(DEDENT)
(ID "list")
(PUNCT "=")
(PUNCT "[")
(PUNCT "]")
(NEWLINE)
(ID "subdirs")
(PUNCT "=")
(PUNCT "[")
(PUNCT "]")
(NEWLINE)
(KEYWORD for)
(ID "name")
(KEYWORD in)
(ID "names")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "fn")
(PUNCT "=")
(ID "os")
(PUNCT ".")
(ID "path")
(PUNCT ".")
(ID "join")
(PUNCT "(")
(ID "dir")
(PUNCT ",")
(ID "name")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(ID "os")
(PUNCT ".")
(ID "path")
(PUNCT ".")
(ID "isdir")
(PUNCT "(")
(ID "fn")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "subdirs")
(PUNCT ".")
(ID "append")
(PUNCT "(")
(ID "fn")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "fnmatch")
(PUNCT ".")
(ID "fnmatch")
(PUNCT "(")
(ID "name")
(PUNCT ",")
(ID "base")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "list")
(PUNCT ".")
(ID "append")
(PUNCT "(")
(ID "fn")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(DEDENT)
(KEYWORD if)
(ID "rec")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD for)
(ID "subdir")
(KEYWORD in)
(ID "subdirs")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "list")
(PUNCT ".")
(ID "extend")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "findfiles")
(PUNCT "(")
(ID "subdir")
(PUNCT ",")
(ID "base")
(PUNCT ",")
(ID "rec")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD return)
(ID "list")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "close")
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
(ID "top")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "top")
(PUNCT ".")
(ID "grab_release")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "top")
(PUNCT ".")
(ID "withdraw")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(DEDENT)
(KEYWORD def)
(ID "_grep_dialog")
(PUNCT "(")
(ID "parent")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD from)
(ID "idlelib")
(PUNCT ".")
(ID "PyShell")
(KEYWORD import)
(ID "PyShellFileList")
(NEWLINE)
(ID "root")
(PUNCT "=")
(ID "Tk")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "root")
(PUNCT ".")
(ID "title")
(PUNCT "(")
(LIT "Test GrepDialog")
(PUNCT ")")
(NEWLINE)
(ID "width")
(PUNCT ",")
(ID "height")
(PUNCT ",")
(ID "x")
(PUNCT ",")
(ID "y")
(PUNCT "=")
(ID "list")
(PUNCT "(")
(ID "map")
(PUNCT "(")
(ID "int")
(PUNCT ",")
(ID "re")
(PUNCT ".")
(ID "split")
(PUNCT "(")
(LIT "[x+]")
(PUNCT ",")
(ID "parent")
(PUNCT ".")
(ID "geometry")
(PUNCT "(")
(PUNCT ")")
(PUNCT ")")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(ID "root")
(PUNCT ".")
(ID "geometry")
(PUNCT "(")
(LIT "+%d+%d")
(PUNCT "%")
(PUNCT "(")
(ID "x")
(PUNCT ",")
(ID "y")
(PUNCT "+")
(LIT 150)
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(ID "flist")
(PUNCT "=")
(ID "PyShellFileList")
(PUNCT "(")
(ID "root")
(PUNCT ")")
(NEWLINE)
(ID "text")
(PUNCT "=")
(ID "Text")
(PUNCT "(")
(ID "root")
(PUNCT ",")
(ID "height")
(PUNCT "=")
(LIT 5)
(PUNCT ")")
(NEWLINE)
(ID "text")
(PUNCT ".")
(ID "pack")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(KEYWORD def)
(ID "show_grep_dialog")
(PUNCT "(")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "text")
(PUNCT ".")
(ID "tag_add")
(PUNCT "(")
(ID "SEL")
(PUNCT ",")
(LIT "1.0")
(PUNCT ",")
(ID "END")
(PUNCT ")")
(NEWLINE)
(ID "grep")
(PUNCT "(")
(ID "text")
(PUNCT ",")
(ID "flist")
(PUNCT "=")
(ID "flist")
(PUNCT ")")
(NEWLINE)
(ID "text")
(PUNCT ".")
(ID "tag_remove")
(PUNCT "(")
(ID "SEL")
(PUNCT ",")
(LIT "1.0")
(PUNCT ",")
(ID "END")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "button")
(PUNCT "=")
(ID "Button")
(PUNCT "(")
(ID "root")
(PUNCT ",")
(ID "text")
(PUNCT "=")
(LIT "Show GrepDialog")
(PUNCT ",")
(ID "command")
(PUNCT "=")
(ID "show_grep_dialog")
(PUNCT ")")
(NEWLINE)
(ID "button")
(PUNCT ".")
(ID "pack")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "root")
(PUNCT ".")
(ID "mainloop")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD if)
(ID "__name__")
(PUNCT "==")
(LIT "__main__")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD import)
(ID "unittest")
(NEWLINE)
(ID "unittest")
(PUNCT ".")
(ID "main")
(PUNCT "(")
(LIT "idlelib.idle_test.test_grep")
(PUNCT ",")
(ID "verbosity")
(PUNCT "=")
(LIT 2)
(PUNCT ",")
(ID "exit")
(PUNCT "=")
(KEYWORD False)
(PUNCT ")")
(NEWLINE)
(KEYWORD from)
(ID "idlelib")
(PUNCT ".")
(ID "idle_test")
(PUNCT ".")
(ID "htest")
(KEYWORD import)
(ID "run")
(NEWLINE)
(ID "run")
(PUNCT "(")
(ID "_grep_dialog")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ENDMARKER)