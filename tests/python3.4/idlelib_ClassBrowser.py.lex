(LIT "Class browser.\n\nXXX TO DO:\n\n- reparse when source changed (maybe just a button would be OK?)\n    (or recheck on window popup)\n- add popup menu with more options (e.g. doc strings, base classes, imports)\n- show function argument list? (have to do pattern matching on source)\n- should the classes and methods lists also be in the module's menu bar?\n- add base classes to class browser tree\n")
(NEWLINE)
(KEYWORD import)
(ID "os")
(NEWLINE)
(KEYWORD import)
(ID "sys")
(NEWLINE)
(KEYWORD import)
(ID "pyclbr")
(NEWLINE)
(KEYWORD from)
(ID "idlelib")
(KEYWORD import)
(ID "PyShell")
(NEWLINE)
(KEYWORD from)
(ID "idlelib")
(PUNCT ".")
(ID "WindowList")
(KEYWORD import)
(ID "ListedToplevel")
(NEWLINE)
(KEYWORD from)
(ID "idlelib")
(PUNCT ".")
(ID "TreeWidget")
(KEYWORD import)
(ID "TreeNode")
(PUNCT ",")
(ID "TreeItem")
(PUNCT ",")
(ID "ScrolledCanvas")
(NEWLINE)
(KEYWORD from)
(ID "idlelib")
(PUNCT ".")
(ID "configHandler")
(KEYWORD import)
(ID "idleConf")
(NEWLINE)
(ID "file_open")
(PUNCT "=")
(KEYWORD None)
(NEWLINE)
(KEYWORD class)
(ID "ClassBrowser")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD def)
(ID "__init__")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "flist")
(PUNCT ",")
(ID "name")
(PUNCT ",")
(ID "path")
(PUNCT ",")
(ID "_htest")
(PUNCT "=")
(KEYWORD False)
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "\n        _htest - bool, change box when location running htest.\n        ")
(NEWLINE)
(KEYWORD global)
(ID "file_open")
(NEWLINE)
(KEYWORD if)
(KEYWORD not)
(ID "_htest")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "file_open")
(PUNCT "=")
(ID "PyShell")
(PUNCT ".")
(ID "flist")
(PUNCT ".")
(ID "open")
(NEWLINE)
(DEDENT)
(ID "self")
(PUNCT ".")
(ID "name")
(PUNCT "=")
(ID "name")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "file")
(PUNCT "=")
(ID "os")
(PUNCT ".")
(ID "path")
(PUNCT ".")
(ID "join")
(PUNCT "(")
(ID "path")
(PUNCT "[")
(LIT 0)
(PUNCT "]")
(PUNCT ",")
(ID "self")
(PUNCT ".")
(ID "name")
(PUNCT "+")
(LIT ".py")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_htest")
(PUNCT "=")
(ID "_htest")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "init")
(PUNCT "(")
(ID "flist")
(PUNCT ")")
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
(ID "self")
(PUNCT ".")
(ID "top")
(PUNCT ".")
(ID "destroy")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "node")
(PUNCT ".")
(ID "destroy")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "init")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "flist")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "flist")
(PUNCT "=")
(ID "flist")
(NEWLINE)
(ID "pyclbr")
(PUNCT ".")
(ID "_modules")
(PUNCT ".")
(ID "clear")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "top")
(PUNCT "=")
(ID "top")
(PUNCT "=")
(ID "ListedToplevel")
(PUNCT "(")
(ID "flist")
(PUNCT ".")
(ID "root")
(PUNCT ")")
(NEWLINE)
(ID "top")
(PUNCT ".")
(ID "protocol")
(PUNCT "(")
(LIT "WM_DELETE_WINDOW")
(PUNCT ",")
(ID "self")
(PUNCT ".")
(ID "close")
(PUNCT ")")
(NEWLINE)
(ID "top")
(PUNCT ".")
(ID "bind")
(PUNCT "(")
(LIT "<Escape>")
(PUNCT ",")
(ID "self")
(PUNCT ".")
(ID "close")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(ID "self")
(PUNCT ".")
(ID "_htest")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "top")
(PUNCT ".")
(ID "geometry")
(PUNCT "(")
(LIT "+%d+%d")
(PUNCT "%")
(PUNCT "(")
(ID "flist")
(PUNCT ".")
(ID "root")
(PUNCT ".")
(ID "winfo_rootx")
(PUNCT "(")
(PUNCT ")")
(PUNCT ",")
(ID "flist")
(PUNCT ".")
(ID "root")
(PUNCT ".")
(ID "winfo_rooty")
(PUNCT "(")
(PUNCT ")")
(PUNCT "+")
(LIT 200)
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "self")
(PUNCT ".")
(ID "settitle")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "top")
(PUNCT ".")
(ID "focus_set")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "theme")
(PUNCT "=")
(ID "idleConf")
(PUNCT ".")
(ID "GetOption")
(PUNCT "(")
(LIT "main")
(PUNCT ",")
(LIT "Theme")
(PUNCT ",")
(LIT "name")
(PUNCT ")")
(NEWLINE)
(ID "background")
(PUNCT "=")
(ID "idleConf")
(PUNCT ".")
(ID "GetHighlight")
(PUNCT "(")
(ID "theme")
(PUNCT ",")
(LIT "normal")
(PUNCT ")")
(PUNCT "[")
(LIT "background")
(PUNCT "]")
(NEWLINE)
(ID "sc")
(PUNCT "=")
(ID "ScrolledCanvas")
(PUNCT "(")
(ID "top")
(PUNCT ",")
(ID "bg")
(PUNCT "=")
(ID "background")
(PUNCT ",")
(ID "highlightthickness")
(PUNCT "=")
(LIT 0)
(PUNCT ",")
(ID "takefocus")
(PUNCT "=")
(LIT 1)
(PUNCT ")")
(NEWLINE)
(ID "sc")
(PUNCT ".")
(ID "frame")
(PUNCT ".")
(ID "pack")
(PUNCT "(")
(ID "expand")
(PUNCT "=")
(LIT 1)
(PUNCT ",")
(ID "fill")
(PUNCT "=")
(LIT "both")
(PUNCT ")")
(NEWLINE)
(ID "item")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "rootnode")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "node")
(PUNCT "=")
(ID "node")
(PUNCT "=")
(ID "TreeNode")
(PUNCT "(")
(ID "sc")
(PUNCT ".")
(ID "canvas")
(PUNCT ",")
(KEYWORD None)
(PUNCT ",")
(ID "item")
(PUNCT ")")
(NEWLINE)
(ID "node")
(PUNCT ".")
(ID "update")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "node")
(PUNCT ".")
(ID "expand")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "settitle")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "top")
(PUNCT ".")
(ID "wm_title")
(PUNCT "(")
(LIT "Class Browser - ")
(PUNCT "+")
(ID "self")
(PUNCT ".")
(ID "name")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "top")
(PUNCT ".")
(ID "wm_iconname")
(PUNCT "(")
(LIT "Class Browser")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "rootnode")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(ID "ModuleBrowserTreeItem")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "file")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD class)
(ID "ModuleBrowserTreeItem")
(PUNCT "(")
(ID "TreeItem")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD def)
(ID "__init__")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "file")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "file")
(PUNCT "=")
(ID "file")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "GetText")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(ID "os")
(PUNCT ".")
(ID "path")
(PUNCT ".")
(ID "basename")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "file")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "GetIconName")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(LIT "python")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "GetSubList")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "sublist")
(PUNCT "=")
(PUNCT "[")
(PUNCT "]")
(NEWLINE)
(KEYWORD for)
(ID "name")
(KEYWORD in)
(ID "self")
(PUNCT ".")
(ID "listclasses")
(PUNCT "(")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "item")
(PUNCT "=")
(ID "ClassBrowserTreeItem")
(PUNCT "(")
(ID "name")
(PUNCT ",")
(ID "self")
(PUNCT ".")
(ID "classes")
(PUNCT ",")
(ID "self")
(PUNCT ".")
(ID "file")
(PUNCT ")")
(NEWLINE)
(ID "sublist")
(PUNCT ".")
(ID "append")
(PUNCT "(")
(ID "item")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD return)
(ID "sublist")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "OnDoubleClick")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "os")
(PUNCT ".")
(ID "path")
(PUNCT ".")
(ID "normcase")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "file")
(PUNCT "[")
(PUNCT "-")
(LIT 3)
(PUNCT ":")
(PUNCT "]")
(PUNCT ")")
(PUNCT "!=")
(LIT ".py")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(NEWLINE)
(DEDENT)
(KEYWORD if)
(KEYWORD not)
(ID "os")
(PUNCT ".")
(ID "path")
(PUNCT ".")
(ID "exists")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "file")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(NEWLINE)
(DEDENT)
(ID "PyShell")
(PUNCT ".")
(ID "flist")
(PUNCT ".")
(ID "open")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "file")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "IsExpandable")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(ID "os")
(PUNCT ".")
(ID "path")
(PUNCT ".")
(ID "normcase")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "file")
(PUNCT "[")
(PUNCT "-")
(LIT 3)
(PUNCT ":")
(PUNCT "]")
(PUNCT ")")
(PUNCT "==")
(LIT ".py")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "listclasses")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "dir")
(PUNCT ",")
(ID "file")
(PUNCT "=")
(ID "os")
(PUNCT ".")
(ID "path")
(PUNCT ".")
(ID "split")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "file")
(PUNCT ")")
(NEWLINE)
(ID "name")
(PUNCT ",")
(ID "ext")
(PUNCT "=")
(ID "os")
(PUNCT ".")
(ID "path")
(PUNCT ".")
(ID "splitext")
(PUNCT "(")
(ID "file")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(ID "os")
(PUNCT ".")
(ID "path")
(PUNCT ".")
(ID "normcase")
(PUNCT "(")
(ID "ext")
(PUNCT ")")
(PUNCT "!=")
(LIT ".py")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(PUNCT "[")
(PUNCT "]")
(NEWLINE)
(DEDENT)
(KEYWORD try)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "dict")
(PUNCT "=")
(ID "pyclbr")
(PUNCT ".")
(ID "readmodule_ex")
(PUNCT "(")
(ID "name")
(PUNCT ",")
(PUNCT "[")
(ID "dir")
(PUNCT "]")
(PUNCT "+")
(ID "sys")
(PUNCT ".")
(ID "path")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD except)
(ID "ImportError")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(PUNCT "[")
(PUNCT "]")
(NEWLINE)
(DEDENT)
(ID "items")
(PUNCT "=")
(PUNCT "[")
(PUNCT "]")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "classes")
(PUNCT "=")
(PUNCT "{")
(PUNCT "}")
(NEWLINE)
(KEYWORD for)
(ID "key")
(PUNCT ",")
(ID "cl")
(KEYWORD in)
(ID "dict")
(PUNCT ".")
(ID "items")
(PUNCT "(")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "cl")
(PUNCT ".")
(ID "module")
(PUNCT "==")
(ID "name")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "s")
(PUNCT "=")
(ID "key")
(NEWLINE)
(KEYWORD if)
(ID "hasattr")
(PUNCT "(")
(ID "cl")
(PUNCT ",")
(LIT "super")
(PUNCT ")")
(KEYWORD and)
(ID "cl")
(PUNCT ".")
(ID "super")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "supers")
(PUNCT "=")
(PUNCT "[")
(PUNCT "]")
(NEWLINE)
(KEYWORD for)
(ID "sup")
(KEYWORD in)
(ID "cl")
(PUNCT ".")
(ID "super")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "type")
(PUNCT "(")
(ID "sup")
(PUNCT ")")
(KEYWORD is)
(ID "type")
(PUNCT "(")
(LIT "")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "sname")
(PUNCT "=")
(ID "sup")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "sname")
(PUNCT "=")
(ID "sup")
(PUNCT ".")
(ID "name")
(NEWLINE)
(KEYWORD if)
(ID "sup")
(PUNCT ".")
(ID "module")
(PUNCT "!=")
(ID "cl")
(PUNCT ".")
(ID "module")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "sname")
(PUNCT "=")
(LIT "%s.%s")
(PUNCT "%")
(PUNCT "(")
(ID "sup")
(PUNCT ".")
(ID "module")
(PUNCT ",")
(ID "sname")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(ID "supers")
(PUNCT ".")
(ID "append")
(PUNCT "(")
(ID "sname")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "s")
(PUNCT "=")
(ID "s")
(PUNCT "+")
(LIT "(%s)")
(PUNCT "%")
(LIT ", ")
(PUNCT ".")
(ID "join")
(PUNCT "(")
(ID "supers")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "items")
(PUNCT ".")
(ID "append")
(PUNCT "(")
(PUNCT "(")
(ID "cl")
(PUNCT ".")
(ID "lineno")
(PUNCT ",")
(ID "s")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "classes")
(PUNCT "[")
(ID "s")
(PUNCT "]")
(PUNCT "=")
(ID "cl")
(NEWLINE)
(DEDENT)
(DEDENT)
(ID "items")
(PUNCT ".")
(ID "sort")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "list")
(PUNCT "=")
(PUNCT "[")
(PUNCT "]")
(NEWLINE)
(KEYWORD for)
(ID "item")
(PUNCT ",")
(ID "s")
(KEYWORD in)
(ID "items")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "list")
(PUNCT ".")
(ID "append")
(PUNCT "(")
(ID "s")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD return)
(ID "list")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD class)
(ID "ClassBrowserTreeItem")
(PUNCT "(")
(ID "TreeItem")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD def)
(ID "__init__")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "name")
(PUNCT ",")
(ID "classes")
(PUNCT ",")
(ID "file")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "name")
(PUNCT "=")
(ID "name")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "classes")
(PUNCT "=")
(ID "classes")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "file")
(PUNCT "=")
(ID "file")
(NEWLINE)
(KEYWORD try)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "cl")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "classes")
(PUNCT "[")
(ID "self")
(PUNCT ".")
(ID "name")
(PUNCT "]")
(NEWLINE)
(DEDENT)
(KEYWORD except)
(PUNCT "(")
(ID "IndexError")
(PUNCT ",")
(ID "KeyError")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "cl")
(PUNCT "=")
(KEYWORD None)
(NEWLINE)
(DEDENT)
(ID "self")
(PUNCT ".")
(ID "isfunction")
(PUNCT "=")
(ID "isinstance")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "cl")
(PUNCT ",")
(ID "pyclbr")
(PUNCT ".")
(ID "Function")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "GetText")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "self")
(PUNCT ".")
(ID "isfunction")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(LIT "def ")
(PUNCT "+")
(ID "self")
(PUNCT ".")
(ID "name")
(PUNCT "+")
(LIT "(...)")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(LIT "class ")
(PUNCT "+")
(ID "self")
(PUNCT ".")
(ID "name")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD def)
(ID "GetIconName")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "self")
(PUNCT ".")
(ID "isfunction")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(LIT "python")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(LIT "folder")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD def)
(ID "IsExpandable")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "self")
(PUNCT ".")
(ID "cl")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD try)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(KEYWORD not)
(KEYWORD not)
(ID "self")
(PUNCT ".")
(ID "cl")
(PUNCT ".")
(ID "methods")
(NEWLINE)
(DEDENT)
(KEYWORD except)
(ID "AttributeError")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(KEYWORD False)
(NEWLINE)
(DEDENT)
(DEDENT)
(DEDENT)
(KEYWORD def)
(ID "GetSubList")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(KEYWORD not)
(ID "self")
(PUNCT ".")
(ID "cl")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(PUNCT "[")
(PUNCT "]")
(NEWLINE)
(DEDENT)
(ID "sublist")
(PUNCT "=")
(PUNCT "[")
(PUNCT "]")
(NEWLINE)
(KEYWORD for)
(ID "name")
(KEYWORD in)
(ID "self")
(PUNCT ".")
(ID "listmethods")
(PUNCT "(")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "item")
(PUNCT "=")
(ID "MethodBrowserTreeItem")
(PUNCT "(")
(ID "name")
(PUNCT ",")
(ID "self")
(PUNCT ".")
(ID "cl")
(PUNCT ",")
(ID "self")
(PUNCT ".")
(ID "file")
(PUNCT ")")
(NEWLINE)
(ID "sublist")
(PUNCT ".")
(ID "append")
(PUNCT "(")
(ID "item")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD return)
(ID "sublist")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "OnDoubleClick")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(KEYWORD not)
(ID "os")
(PUNCT ".")
(ID "path")
(PUNCT ".")
(ID "exists")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "file")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(NEWLINE)
(DEDENT)
(ID "edit")
(PUNCT "=")
(ID "file_open")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "file")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(ID "hasattr")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "cl")
(PUNCT ",")
(LIT "lineno")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "lineno")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "cl")
(PUNCT ".")
(ID "lineno")
(NEWLINE)
(ID "edit")
(PUNCT ".")
(ID "gotoline")
(PUNCT "(")
(ID "lineno")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD def)
(ID "listmethods")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(KEYWORD not)
(ID "self")
(PUNCT ".")
(ID "cl")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(PUNCT "[")
(PUNCT "]")
(NEWLINE)
(DEDENT)
(ID "items")
(PUNCT "=")
(PUNCT "[")
(PUNCT "]")
(NEWLINE)
(KEYWORD for)
(ID "name")
(PUNCT ",")
(ID "lineno")
(KEYWORD in)
(ID "self")
(PUNCT ".")
(ID "cl")
(PUNCT ".")
(ID "methods")
(PUNCT ".")
(ID "items")
(PUNCT "(")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "items")
(PUNCT ".")
(ID "append")
(PUNCT "(")
(PUNCT "(")
(ID "lineno")
(PUNCT ",")
(ID "name")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "items")
(PUNCT ".")
(ID "sort")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "list")
(PUNCT "=")
(PUNCT "[")
(PUNCT "]")
(NEWLINE)
(KEYWORD for)
(ID "item")
(PUNCT ",")
(ID "name")
(KEYWORD in)
(ID "items")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "list")
(PUNCT ".")
(ID "append")
(PUNCT "(")
(ID "name")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD return)
(ID "list")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD class)
(ID "MethodBrowserTreeItem")
(PUNCT "(")
(ID "TreeItem")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD def)
(ID "__init__")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "name")
(PUNCT ",")
(ID "cl")
(PUNCT ",")
(ID "file")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "name")
(PUNCT "=")
(ID "name")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "cl")
(PUNCT "=")
(ID "cl")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "file")
(PUNCT "=")
(ID "file")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "GetText")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(LIT "def ")
(PUNCT "+")
(ID "self")
(PUNCT ".")
(ID "name")
(PUNCT "+")
(LIT "(...)")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "GetIconName")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(LIT "python")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "IsExpandable")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(LIT 0)
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "OnDoubleClick")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(KEYWORD not)
(ID "os")
(PUNCT ".")
(ID "path")
(PUNCT ".")
(ID "exists")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "file")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(NEWLINE)
(DEDENT)
(ID "edit")
(PUNCT "=")
(ID "file_open")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "file")
(PUNCT ")")
(NEWLINE)
(ID "edit")
(PUNCT ".")
(ID "gotoline")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "cl")
(PUNCT ".")
(ID "methods")
(PUNCT "[")
(ID "self")
(PUNCT ".")
(ID "name")
(PUNCT "]")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD def)
(ID "_class_browser")
(PUNCT "(")
(ID "parent")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD try)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "file")
(PUNCT "=")
(ID "__file__")
(NEWLINE)
(DEDENT)
(KEYWORD except)
(ID "NameError")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "file")
(PUNCT "=")
(ID "sys")
(PUNCT ".")
(ID "argv")
(PUNCT "[")
(LIT 0)
(PUNCT "]")
(NEWLINE)
(KEYWORD if)
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
(ID "file")
(PUNCT "=")
(ID "sys")
(PUNCT ".")
(ID "argv")
(PUNCT "[")
(LIT 1)
(PUNCT "]")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "file")
(PUNCT "=")
(ID "sys")
(PUNCT ".")
(ID "argv")
(PUNCT "[")
(LIT 0)
(PUNCT "]")
(NEWLINE)
(DEDENT)
(DEDENT)
(ID "dir")
(PUNCT ",")
(ID "file")
(PUNCT "=")
(ID "os")
(PUNCT ".")
(ID "path")
(PUNCT ".")
(ID "split")
(PUNCT "(")
(ID "file")
(PUNCT ")")
(NEWLINE)
(ID "name")
(PUNCT "=")
(ID "os")
(PUNCT ".")
(ID "path")
(PUNCT ".")
(ID "splitext")
(PUNCT "(")
(ID "file")
(PUNCT ")")
(PUNCT "[")
(LIT 0)
(PUNCT "]")
(NEWLINE)
(ID "flist")
(PUNCT "=")
(ID "PyShell")
(PUNCT ".")
(ID "PyShellFileList")
(PUNCT "(")
(ID "parent")
(PUNCT ")")
(NEWLINE)
(KEYWORD global)
(ID "file_open")
(NEWLINE)
(ID "file_open")
(PUNCT "=")
(ID "flist")
(PUNCT ".")
(ID "open")
(NEWLINE)
(ID "ClassBrowser")
(PUNCT "(")
(ID "flist")
(PUNCT ",")
(ID "name")
(PUNCT ",")
(PUNCT "[")
(ID "dir")
(PUNCT "]")
(PUNCT ",")
(ID "_htest")
(PUNCT "=")
(KEYWORD True)
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
(ID "_class_browser")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ENDMARKER)
