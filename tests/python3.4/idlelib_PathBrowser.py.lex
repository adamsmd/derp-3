(KEYWORD import)
(ID "os")
(NEWLINE)
(KEYWORD import)
(ID "sys")
(NEWLINE)
(KEYWORD import)
(ID "importlib")
(PUNCT ".")
(ID "machinery")
(NEWLINE)
(KEYWORD from)
(ID "idlelib")
(PUNCT ".")
(ID "TreeWidget")
(KEYWORD import)
(ID "TreeItem")
(NEWLINE)
(KEYWORD from)
(ID "idlelib")
(PUNCT ".")
(ID "ClassBrowser")
(KEYWORD import)
(ID "ClassBrowser")
(PUNCT ",")
(ID "ModuleBrowserTreeItem")
(NEWLINE)
(KEYWORD from)
(ID "idlelib")
(PUNCT ".")
(ID "PyShell")
(KEYWORD import)
(ID "PyShellFileList")
(NEWLINE)
(KEYWORD class)
(ID "PathBrowser")
(PUNCT "(")
(ID "ClassBrowser")
(PUNCT ")")
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
(ID "_htest")
(PUNCT "=")
(KEYWORD False)
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "\n        _htest - bool, change box location when running htest\n        ")
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
(LIT "Path Browser")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "top")
(PUNCT ".")
(ID "wm_iconname")
(PUNCT "(")
(LIT "Path Browser")
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
(ID "PathBrowserTreeItem")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD class)
(ID "PathBrowserTreeItem")
(PUNCT "(")
(ID "TreeItem")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD def)
(ID "GetText")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(LIT "sys.path")
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
(ID "dir")
(KEYWORD in)
(ID "sys")
(PUNCT ".")
(ID "path")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "item")
(PUNCT "=")
(ID "DirBrowserTreeItem")
(PUNCT "(")
(ID "dir")
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
(DEDENT)
(KEYWORD class)
(ID "DirBrowserTreeItem")
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
(ID "dir")
(PUNCT ",")
(ID "packages")
(PUNCT "=")
(PUNCT "[")
(PUNCT "]")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "dir")
(PUNCT "=")
(ID "dir")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "packages")
(PUNCT "=")
(ID "packages")
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
(KEYWORD not)
(ID "self")
(PUNCT ".")
(ID "packages")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(ID "self")
(PUNCT ".")
(ID "dir")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(ID "self")
(PUNCT ".")
(ID "packages")
(PUNCT "[")
(PUNCT "-")
(LIT 1)
(PUNCT "]")
(PUNCT "+")
(LIT ": package")
(NEWLINE)
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
(ID "self")
(PUNCT ".")
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
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(PUNCT "[")
(PUNCT "]")
(NEWLINE)
(DEDENT)
(ID "packages")
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
(ID "file")
(PUNCT "=")
(ID "os")
(PUNCT ".")
(ID "path")
(PUNCT ".")
(ID "join")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "dir")
(PUNCT ",")
(ID "name")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(ID "self")
(PUNCT ".")
(ID "ispackagedir")
(PUNCT "(")
(ID "file")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "nn")
(PUNCT "=")
(ID "os")
(PUNCT ".")
(ID "path")
(PUNCT ".")
(ID "normcase")
(PUNCT "(")
(ID "name")
(PUNCT ")")
(NEWLINE)
(ID "packages")
(PUNCT ".")
(ID "append")
(PUNCT "(")
(PUNCT "(")
(ID "nn")
(PUNCT ",")
(ID "name")
(PUNCT ",")
(ID "file")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(ID "packages")
(PUNCT ".")
(ID "sort")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "sublist")
(PUNCT "=")
(PUNCT "[")
(PUNCT "]")
(NEWLINE)
(KEYWORD for)
(ID "nn")
(PUNCT ",")
(ID "name")
(PUNCT ",")
(ID "file")
(KEYWORD in)
(ID "packages")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "item")
(PUNCT "=")
(ID "DirBrowserTreeItem")
(PUNCT "(")
(ID "file")
(PUNCT ",")
(ID "self")
(PUNCT ".")
(ID "packages")
(PUNCT "+")
(PUNCT "[")
(ID "name")
(PUNCT "]")
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
(KEYWORD for)
(ID "nn")
(PUNCT ",")
(ID "name")
(KEYWORD in)
(ID "self")
(PUNCT ".")
(ID "listmodules")
(PUNCT "(")
(ID "names")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "item")
(PUNCT "=")
(ID "ModuleBrowserTreeItem")
(PUNCT "(")
(ID "os")
(PUNCT ".")
(ID "path")
(PUNCT ".")
(ID "join")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "dir")
(PUNCT ",")
(ID "name")
(PUNCT ")")
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
(ID "ispackagedir")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "file")
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
(ID "isdir")
(PUNCT "(")
(ID "file")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(LIT 0)
(NEWLINE)
(DEDENT)
(ID "init")
(PUNCT "=")
(ID "os")
(PUNCT ".")
(ID "path")
(PUNCT ".")
(ID "join")
(PUNCT "(")
(ID "file")
(PUNCT ",")
(LIT "__init__.py")
(PUNCT ")")
(NEWLINE)
(KEYWORD return)
(ID "os")
(PUNCT ".")
(ID "path")
(PUNCT ".")
(ID "exists")
(PUNCT "(")
(ID "init")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "listmodules")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "allnames")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "modules")
(PUNCT "=")
(PUNCT "{")
(PUNCT "}")
(NEWLINE)
(ID "suffixes")
(PUNCT "=")
(ID "importlib")
(PUNCT ".")
(ID "machinery")
(PUNCT ".")
(ID "EXTENSION_SUFFIXES")
(PUNCT "[")
(PUNCT ":")
(PUNCT "]")
(NEWLINE)
(ID "suffixes")
(PUNCT "+=")
(ID "importlib")
(PUNCT ".")
(ID "machinery")
(PUNCT ".")
(ID "SOURCE_SUFFIXES")
(PUNCT "[")
(PUNCT ":")
(PUNCT "]")
(NEWLINE)
(ID "suffixes")
(PUNCT "+=")
(ID "importlib")
(PUNCT ".")
(ID "machinery")
(PUNCT ".")
(ID "BYTECODE_SUFFIXES")
(PUNCT "[")
(PUNCT ":")
(PUNCT "]")
(NEWLINE)
(ID "sorted")
(PUNCT "=")
(PUNCT "[")
(PUNCT "]")
(NEWLINE)
(KEYWORD for)
(ID "suff")
(KEYWORD in)
(ID "suffixes")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "i")
(PUNCT "=")
(PUNCT "-")
(ID "len")
(PUNCT "(")
(ID "suff")
(PUNCT ")")
(NEWLINE)
(KEYWORD for)
(ID "name")
(KEYWORD in)
(ID "allnames")
(PUNCT "[")
(PUNCT ":")
(PUNCT "]")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "normed_name")
(PUNCT "=")
(ID "os")
(PUNCT ".")
(ID "path")
(PUNCT ".")
(ID "normcase")
(PUNCT "(")
(ID "name")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(ID "normed_name")
(PUNCT "[")
(ID "i")
(PUNCT ":")
(PUNCT "]")
(PUNCT "==")
(ID "suff")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "mod_name")
(PUNCT "=")
(ID "name")
(PUNCT "[")
(PUNCT ":")
(ID "i")
(PUNCT "]")
(NEWLINE)
(KEYWORD if)
(ID "mod_name")
(KEYWORD not)
(KEYWORD in)
(ID "modules")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "modules")
(PUNCT "[")
(ID "mod_name")
(PUNCT "]")
(PUNCT "=")
(KEYWORD None)
(NEWLINE)
(ID "sorted")
(PUNCT ".")
(ID "append")
(PUNCT "(")
(PUNCT "(")
(ID "normed_name")
(PUNCT ",")
(ID "name")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(ID "allnames")
(PUNCT ".")
(ID "remove")
(PUNCT "(")
(ID "name")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(DEDENT)
(DEDENT)
(ID "sorted")
(PUNCT ".")
(ID "sort")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(KEYWORD return)
(ID "sorted")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD def)
(ID "_path_browser")
(PUNCT "(")
(ID "parent")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "flist")
(PUNCT "=")
(ID "PyShellFileList")
(PUNCT "(")
(ID "parent")
(PUNCT ")")
(NEWLINE)
(ID "PathBrowser")
(PUNCT "(")
(ID "flist")
(PUNCT ",")
(ID "_htest")
(PUNCT "=")
(KEYWORD True)
(PUNCT ")")
(NEWLINE)
(ID "parent")
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
(KEYWORD from)
(ID "unittest")
(KEYWORD import)
(ID "main")
(NEWLINE)
(ID "main")
(PUNCT "(")
(LIT "idlelib.idle_test.test_pathbrowser")
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
(ID "_path_browser")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ENDMARKER)
