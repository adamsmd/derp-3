(LIT "distutils.command.install_lib\n\nImplements the Distutils 'install_lib' command\n(install all Python modules).")
(NEWLINE)
(KEYWORD import)
(ID "os")
(NEWLINE)
(KEYWORD import)
(ID "importlib")
(PUNCT ".")
(ID "util")
(NEWLINE)
(KEYWORD import)
(ID "sys")
(NEWLINE)
(KEYWORD from)
(ID "distutils")
(PUNCT ".")
(ID "core")
(KEYWORD import)
(ID "Command")
(NEWLINE)
(KEYWORD from)
(ID "distutils")
(PUNCT ".")
(ID "errors")
(KEYWORD import)
(ID "DistutilsOptionError")
(NEWLINE)
(ID "PYTHON_SOURCE_EXTENSION")
(PUNCT "=")
(LIT ".py")
(NEWLINE)
(KEYWORD class)
(ID "install_lib")
(PUNCT "(")
(ID "Command")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "description")
(PUNCT "=")
(LIT "install all Python modules (extensions and pure Python)")
(NEWLINE)
(ID "user_options")
(PUNCT "=")
(PUNCT "[")
(PUNCT "(")
(LIT "install-dir=")
(PUNCT ",")
(LIT "d")
(PUNCT ",")
(LIT "directory to install to")
(PUNCT ")")
(PUNCT ",")
(PUNCT "(")
(LIT "build-dir=")
(PUNCT ",")
(LIT "b")
(PUNCT ",")
(LIT "build directory (where to install from)")
(PUNCT ")")
(PUNCT ",")
(PUNCT "(")
(LIT "force")
(PUNCT ",")
(LIT "f")
(PUNCT ",")
(LIT "force installation (overwrite existing files)")
(PUNCT ")")
(PUNCT ",")
(PUNCT "(")
(LIT "compile")
(PUNCT ",")
(LIT "c")
(PUNCT ",")
(LIT "compile .py to .pyc [default]")
(PUNCT ")")
(PUNCT ",")
(PUNCT "(")
(LIT "no-compile")
(PUNCT ",")
(KEYWORD None)
(PUNCT ",")
(LIT "don't compile .py files")
(PUNCT ")")
(PUNCT ",")
(PUNCT "(")
(LIT "optimize=")
(PUNCT ",")
(LIT "O")
(PUNCT ",")
(LIT "also compile with optimization: -O1 for \"python -O\", ")
(LIT "-O2 for \"python -OO\", and -O0 to disable [default: -O0]")
(PUNCT ")")
(PUNCT ",")
(PUNCT "(")
(LIT "skip-build")
(PUNCT ",")
(KEYWORD None)
(PUNCT ",")
(LIT "skip the build steps")
(PUNCT ")")
(PUNCT ",")
(PUNCT "]")
(NEWLINE)
(ID "boolean_options")
(PUNCT "=")
(PUNCT "[")
(LIT "force")
(PUNCT ",")
(LIT "compile")
(PUNCT ",")
(LIT "skip-build")
(PUNCT "]")
(NEWLINE)
(ID "negative_opt")
(PUNCT "=")
(PUNCT "{")
(LIT "no-compile")
(PUNCT ":")
(LIT "compile")
(PUNCT "}")
(NEWLINE)
(KEYWORD def)
(ID "initialize_options")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "install_dir")
(PUNCT "=")
(KEYWORD None)
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "build_dir")
(PUNCT "=")
(KEYWORD None)
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "force")
(PUNCT "=")
(LIT 0)
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "compile")
(PUNCT "=")
(KEYWORD None)
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "optimize")
(PUNCT "=")
(KEYWORD None)
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "skip_build")
(PUNCT "=")
(KEYWORD None)
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "multiarch")
(PUNCT "=")
(KEYWORD None)
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "finalize_options")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "set_undefined_options")
(PUNCT "(")
(LIT "install")
(PUNCT ",")
(PUNCT "(")
(LIT "build_lib")
(PUNCT ",")
(LIT "build_dir")
(PUNCT ")")
(PUNCT ",")
(PUNCT "(")
(LIT "install_lib")
(PUNCT ",")
(LIT "install_dir")
(PUNCT ")")
(PUNCT ",")
(PUNCT "(")
(LIT "force")
(PUNCT ",")
(LIT "force")
(PUNCT ")")
(PUNCT ",")
(PUNCT "(")
(LIT "compile")
(PUNCT ",")
(LIT "compile")
(PUNCT ")")
(PUNCT ",")
(PUNCT "(")
(LIT "optimize")
(PUNCT ",")
(LIT "optimize")
(PUNCT ")")
(PUNCT ",")
(PUNCT "(")
(LIT "skip_build")
(PUNCT ",")
(LIT "skip_build")
(PUNCT ")")
(PUNCT ",")
(PUNCT "(")
(LIT "multiarch")
(PUNCT ",")
(LIT "multiarch")
(PUNCT ")")
(PUNCT ",")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(ID "self")
(PUNCT ".")
(ID "compile")
(KEYWORD is)
(KEYWORD None)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "compile")
(PUNCT "=")
(KEYWORD True)
(NEWLINE)
(DEDENT)
(KEYWORD if)
(ID "self")
(PUNCT ".")
(ID "optimize")
(KEYWORD is)
(KEYWORD None)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "optimize")
(PUNCT "=")
(KEYWORD False)
(NEWLINE)
(DEDENT)
(KEYWORD if)
(KEYWORD not)
(ID "isinstance")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "optimize")
(PUNCT ",")
(ID "int")
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
(ID "optimize")
(PUNCT "=")
(ID "int")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "optimize")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(ID "self")
(PUNCT ".")
(ID "optimize")
(KEYWORD not)
(KEYWORD in)
(PUNCT "(")
(LIT 0)
(PUNCT ",")
(LIT 1)
(PUNCT ",")
(LIT 2)
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD raise)
(ID "AssertionError")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD except)
(PUNCT "(")
(ID "ValueError")
(PUNCT ",")
(ID "AssertionError")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD raise)
(ID "DistutilsOptionError")
(PUNCT "(")
(LIT "optimize must be 0, 1, or 2")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(DEDENT)
(KEYWORD def)
(ID "run")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "build")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "outfiles")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "install")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(ID "outfiles")
(KEYWORD is)
(KEYWORD not)
(KEYWORD None)
(KEYWORD and)
(ID "self")
(PUNCT ".")
(ID "distribution")
(PUNCT ".")
(ID "has_pure_modules")
(PUNCT "(")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "byte_compile")
(PUNCT "(")
(ID "outfiles")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD def)
(ID "build")
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
(ID "skip_build")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "self")
(PUNCT ".")
(ID "distribution")
(PUNCT ".")
(ID "has_pure_modules")
(PUNCT "(")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "run_command")
(PUNCT "(")
(LIT "build_py")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD if)
(ID "self")
(PUNCT ".")
(ID "distribution")
(PUNCT ".")
(ID "has_ext_modules")
(PUNCT "(")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "run_command")
(PUNCT "(")
(LIT "build_ext")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(DEDENT)
(KEYWORD def)
(ID "install")
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
(ID "isdir")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "build_dir")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD import)
(ID "distutils")
(PUNCT ".")
(ID "dir_util")
(NEWLINE)
(ID "distutils")
(PUNCT ".")
(ID "dir_util")
(PUNCT ".")
(ID "_multiarch")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "multiarch")
(NEWLINE)
(ID "outfiles")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "copy_tree")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "build_dir")
(PUNCT ",")
(ID "self")
(PUNCT ".")
(ID "install_dir")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "warn")
(PUNCT "(")
(LIT "'%s' does not exist -- no Python modules to install")
(PUNCT "%")
(ID "self")
(PUNCT ".")
(ID "build_dir")
(PUNCT ")")
(NEWLINE)
(KEYWORD return)
(NEWLINE)
(DEDENT)
(KEYWORD return)
(ID "outfiles")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "byte_compile")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "files")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "sys")
(PUNCT ".")
(ID "dont_write_bytecode")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "warn")
(PUNCT "(")
(LIT "byte-compiling is disabled, skipping.")
(PUNCT ")")
(NEWLINE)
(KEYWORD return)
(NEWLINE)
(DEDENT)
(KEYWORD from)
(ID "distutils")
(PUNCT ".")
(ID "util")
(KEYWORD import)
(ID "byte_compile")
(NEWLINE)
(ID "install_root")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "get_finalized_command")
(PUNCT "(")
(LIT "install")
(PUNCT ")")
(PUNCT ".")
(ID "root")
(NEWLINE)
(KEYWORD if)
(ID "self")
(PUNCT ".")
(ID "compile")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "byte_compile")
(PUNCT "(")
(ID "files")
(PUNCT ",")
(ID "optimize")
(PUNCT "=")
(LIT 0)
(PUNCT ",")
(ID "force")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "force")
(PUNCT ",")
(ID "prefix")
(PUNCT "=")
(ID "install_root")
(PUNCT ",")
(ID "dry_run")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "dry_run")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD if)
(ID "self")
(PUNCT ".")
(ID "optimize")
(PUNCT ">")
(LIT 0)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "byte_compile")
(PUNCT "(")
(ID "files")
(PUNCT ",")
(ID "optimize")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "optimize")
(PUNCT ",")
(ID "force")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "force")
(PUNCT ",")
(ID "prefix")
(PUNCT "=")
(ID "install_root")
(PUNCT ",")
(ID "verbose")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "verbose")
(PUNCT ",")
(ID "dry_run")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "dry_run")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD def)
(ID "_mutate_outputs")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "has_any")
(PUNCT ",")
(ID "build_cmd")
(PUNCT ",")
(ID "cmd_option")
(PUNCT ",")
(ID "output_dir")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(KEYWORD not)
(ID "has_any")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(PUNCT "[")
(PUNCT "]")
(NEWLINE)
(DEDENT)
(ID "build_cmd")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "get_finalized_command")
(PUNCT "(")
(ID "build_cmd")
(PUNCT ")")
(NEWLINE)
(ID "build_files")
(PUNCT "=")
(ID "build_cmd")
(PUNCT ".")
(ID "get_outputs")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "build_dir")
(PUNCT "=")
(ID "getattr")
(PUNCT "(")
(ID "build_cmd")
(PUNCT ",")
(ID "cmd_option")
(PUNCT ")")
(NEWLINE)
(ID "prefix_len")
(PUNCT "=")
(ID "len")
(PUNCT "(")
(ID "build_dir")
(PUNCT ")")
(PUNCT "+")
(ID "len")
(PUNCT "(")
(ID "os")
(PUNCT ".")
(ID "sep")
(PUNCT ")")
(NEWLINE)
(ID "outputs")
(PUNCT "=")
(PUNCT "[")
(PUNCT "]")
(NEWLINE)
(KEYWORD for)
(ID "file")
(KEYWORD in)
(ID "build_files")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "outputs")
(PUNCT ".")
(ID "append")
(PUNCT "(")
(ID "os")
(PUNCT ".")
(ID "path")
(PUNCT ".")
(ID "join")
(PUNCT "(")
(ID "output_dir")
(PUNCT ",")
(ID "file")
(PUNCT "[")
(ID "prefix_len")
(PUNCT ":")
(PUNCT "]")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD return)
(ID "outputs")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "_bytecode_filenames")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "py_filenames")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "bytecode_files")
(PUNCT "=")
(PUNCT "[")
(PUNCT "]")
(NEWLINE)
(KEYWORD for)
(ID "py_file")
(KEYWORD in)
(ID "py_filenames")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "ext")
(PUNCT "=")
(ID "os")
(PUNCT ".")
(ID "path")
(PUNCT ".")
(ID "splitext")
(PUNCT "(")
(ID "os")
(PUNCT ".")
(ID "path")
(PUNCT ".")
(ID "normcase")
(PUNCT "(")
(ID "py_file")
(PUNCT ")")
(PUNCT ")")
(PUNCT "[")
(LIT 1)
(PUNCT "]")
(NEWLINE)
(KEYWORD if)
(ID "ext")
(PUNCT "!=")
(ID "PYTHON_SOURCE_EXTENSION")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD continue)
(NEWLINE)
(DEDENT)
(KEYWORD if)
(ID "self")
(PUNCT ".")
(ID "compile")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "bytecode_files")
(PUNCT ".")
(ID "append")
(PUNCT "(")
(ID "importlib")
(PUNCT ".")
(ID "util")
(PUNCT ".")
(ID "cache_from_source")
(PUNCT "(")
(ID "py_file")
(PUNCT ",")
(ID "debug_override")
(PUNCT "=")
(KEYWORD True)
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD if)
(ID "self")
(PUNCT ".")
(ID "optimize")
(PUNCT ">")
(LIT 0)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "bytecode_files")
(PUNCT ".")
(ID "append")
(PUNCT "(")
(ID "importlib")
(PUNCT ".")
(ID "util")
(PUNCT ".")
(ID "cache_from_source")
(PUNCT "(")
(ID "py_file")
(PUNCT ",")
(ID "debug_override")
(PUNCT "=")
(KEYWORD False)
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD return)
(ID "bytecode_files")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "get_outputs")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Return the list of files that would be installed if this command\n        were actually run.  Not affected by the \"dry-run\" flag or whether\n        modules have actually been built yet.\n        ")
(NEWLINE)
(ID "pure_outputs")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "_mutate_outputs")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "distribution")
(PUNCT ".")
(ID "has_pure_modules")
(PUNCT "(")
(PUNCT ")")
(PUNCT ",")
(LIT "build_py")
(PUNCT ",")
(LIT "build_lib")
(PUNCT ",")
(ID "self")
(PUNCT ".")
(ID "install_dir")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(ID "self")
(PUNCT ".")
(ID "compile")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "bytecode_outputs")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "_bytecode_filenames")
(PUNCT "(")
(ID "pure_outputs")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "bytecode_outputs")
(PUNCT "=")
(PUNCT "[")
(PUNCT "]")
(NEWLINE)
(DEDENT)
(ID "ext_outputs")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "_mutate_outputs")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "distribution")
(PUNCT ".")
(ID "has_ext_modules")
(PUNCT "(")
(PUNCT ")")
(PUNCT ",")
(LIT "build_ext")
(PUNCT ",")
(LIT "build_lib")
(PUNCT ",")
(ID "self")
(PUNCT ".")
(ID "install_dir")
(PUNCT ")")
(NEWLINE)
(KEYWORD return)
(ID "pure_outputs")
(PUNCT "+")
(ID "bytecode_outputs")
(PUNCT "+")
(ID "ext_outputs")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "get_inputs")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Get the list of files that are input to this command, ie. the\n        files that get installed as they are named in the build tree.\n        The files in this list correspond one-to-one to the output\n        filenames returned by 'get_outputs()'.\n        ")
(NEWLINE)
(ID "inputs")
(PUNCT "=")
(PUNCT "[")
(PUNCT "]")
(NEWLINE)
(KEYWORD if)
(ID "self")
(PUNCT ".")
(ID "distribution")
(PUNCT ".")
(ID "has_pure_modules")
(PUNCT "(")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "build_py")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "get_finalized_command")
(PUNCT "(")
(LIT "build_py")
(PUNCT ")")
(NEWLINE)
(ID "inputs")
(PUNCT ".")
(ID "extend")
(PUNCT "(")
(ID "build_py")
(PUNCT ".")
(ID "get_outputs")
(PUNCT "(")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD if)
(ID "self")
(PUNCT ".")
(ID "distribution")
(PUNCT ".")
(ID "has_ext_modules")
(PUNCT "(")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "build_ext")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "get_finalized_command")
(PUNCT "(")
(LIT "build_ext")
(PUNCT ")")
(NEWLINE)
(ID "inputs")
(PUNCT ".")
(ID "extend")
(PUNCT "(")
(ID "build_ext")
(PUNCT ".")
(ID "get_outputs")
(PUNCT "(")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD return)
(ID "inputs")
(NEWLINE)
(DEDENT)
(DEDENT)
(ENDMARKER)
