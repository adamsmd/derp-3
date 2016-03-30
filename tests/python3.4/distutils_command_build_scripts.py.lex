(LIT "distutils.command.build_scripts\n\nImplements the Distutils 'build_scripts' command.")
(NEWLINE)
(KEYWORD import)
(ID "os")
(PUNCT ",")
(ID "re")
(NEWLINE)
(KEYWORD from)
(ID "stat")
(KEYWORD import)
(ID "ST_MODE")
(NEWLINE)
(KEYWORD from)
(ID "distutils")
(KEYWORD import)
(ID "sysconfig")
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
(ID "dep_util")
(KEYWORD import)
(ID "newer")
(NEWLINE)
(KEYWORD from)
(ID "distutils")
(PUNCT ".")
(ID "util")
(KEYWORD import)
(ID "convert_path")
(PUNCT ",")
(ID "Mixin2to3")
(NEWLINE)
(KEYWORD from)
(ID "distutils")
(KEYWORD import)
(ID "log")
(NEWLINE)
(KEYWORD import)
(ID "tokenize")
(NEWLINE)
(ID "first_line_re")
(PUNCT "=")
(ID "re")
(PUNCT ".")
(ID "compile")
(PUNCT "(")
(LIT #"^#!.*python[0-9.]*([ \t].*)?$")
(PUNCT ")")
(NEWLINE)
(KEYWORD class)
(ID "build_scripts")
(PUNCT "(")
(ID "Command")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "description")
(PUNCT "=")
(LIT "\"build\" scripts (copy and fixup #! line)")
(NEWLINE)
(ID "user_options")
(PUNCT "=")
(PUNCT "[")
(PUNCT "(")
(LIT "build-dir=")
(PUNCT ",")
(LIT "d")
(PUNCT ",")
(LIT "directory to \"build\" (copy) to")
(PUNCT ")")
(PUNCT ",")
(PUNCT "(")
(LIT "force")
(PUNCT ",")
(LIT "f")
(PUNCT ",")
(LIT "forcibly build everything (ignore file timestamps")
(PUNCT ")")
(PUNCT ",")
(PUNCT "(")
(LIT "executable=")
(PUNCT ",")
(LIT "e")
(PUNCT ",")
(LIT "specify final destination interpreter path")
(PUNCT ")")
(PUNCT ",")
(PUNCT "]")
(NEWLINE)
(ID "boolean_options")
(PUNCT "=")
(PUNCT "[")
(LIT "force")
(PUNCT "]")
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
(ID "build_dir")
(PUNCT "=")
(KEYWORD None)
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "scripts")
(PUNCT "=")
(KEYWORD None)
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "force")
(PUNCT "=")
(KEYWORD None)
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "executable")
(PUNCT "=")
(KEYWORD None)
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "outfiles")
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
(LIT "build")
(PUNCT ",")
(PUNCT "(")
(LIT "build_scripts")
(PUNCT ",")
(LIT "build_dir")
(PUNCT ")")
(PUNCT ",")
(PUNCT "(")
(LIT "force")
(PUNCT ",")
(LIT "force")
(PUNCT ")")
(PUNCT ",")
(PUNCT "(")
(LIT "executable")
(PUNCT ",")
(LIT "executable")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "scripts")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "distribution")
(PUNCT ".")
(ID "scripts")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "get_source_files")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(ID "self")
(PUNCT ".")
(ID "scripts")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "run")
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
(ID "scripts")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(NEWLINE)
(DEDENT)
(ID "self")
(PUNCT ".")
(ID "copy_scripts")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "copy_scripts")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Copy each script listed in 'self.scripts'; if it's marked as a\n        Python script in the Unix way (first line matches 'first_line_re',\n        ie. starts with \"\\#!\" and contains \"python\"), then adjust the first\n        line to refer to the current Python interpreter as we copy.\n        ")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "mkpath")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "build_dir")
(PUNCT ")")
(NEWLINE)
(ID "outfiles")
(PUNCT "=")
(PUNCT "[")
(PUNCT "]")
(NEWLINE)
(ID "updated_files")
(PUNCT "=")
(PUNCT "[")
(PUNCT "]")
(NEWLINE)
(KEYWORD for)
(ID "script")
(KEYWORD in)
(ID "self")
(PUNCT ".")
(ID "scripts")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "adjust")
(PUNCT "=")
(KEYWORD False)
(NEWLINE)
(ID "script")
(PUNCT "=")
(ID "convert_path")
(PUNCT "(")
(ID "script")
(PUNCT ")")
(NEWLINE)
(ID "outfile")
(PUNCT "=")
(ID "os")
(PUNCT ".")
(ID "path")
(PUNCT ".")
(ID "join")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "build_dir")
(PUNCT ",")
(ID "os")
(PUNCT ".")
(ID "path")
(PUNCT ".")
(ID "basename")
(PUNCT "(")
(ID "script")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(ID "outfiles")
(PUNCT ".")
(ID "append")
(PUNCT "(")
(ID "outfile")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(KEYWORD not)
(ID "self")
(PUNCT ".")
(ID "force")
(KEYWORD and)
(KEYWORD not)
(ID "newer")
(PUNCT "(")
(ID "script")
(PUNCT ",")
(ID "outfile")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "log")
(PUNCT ".")
(ID "debug")
(PUNCT "(")
(LIT "not copying %s (up-to-date)")
(PUNCT ",")
(ID "script")
(PUNCT ")")
(NEWLINE)
(KEYWORD continue)
(NEWLINE)
(DEDENT)
(KEYWORD try)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "f")
(PUNCT "=")
(ID "open")
(PUNCT "(")
(ID "script")
(PUNCT ",")
(LIT "rb")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD except)
(ID "OSError")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(KEYWORD not)
(ID "self")
(PUNCT ".")
(ID "dry_run")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD raise)
(NEWLINE)
(DEDENT)
(ID "f")
(PUNCT "=")
(KEYWORD None)
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "encoding")
(PUNCT ",")
(ID "lines")
(PUNCT "=")
(ID "tokenize")
(PUNCT ".")
(ID "detect_encoding")
(PUNCT "(")
(ID "f")
(PUNCT ".")
(ID "readline")
(PUNCT ")")
(NEWLINE)
(ID "f")
(PUNCT ".")
(ID "seek")
(PUNCT "(")
(LIT 0)
(PUNCT ")")
(NEWLINE)
(ID "first_line")
(PUNCT "=")
(ID "f")
(PUNCT ".")
(ID "readline")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(KEYWORD not)
(ID "first_line")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "warn")
(PUNCT "(")
(LIT "%s is an empty file (skipping)")
(PUNCT "%")
(ID "script")
(PUNCT ")")
(NEWLINE)
(KEYWORD continue)
(NEWLINE)
(DEDENT)
(ID "match")
(PUNCT "=")
(ID "first_line_re")
(PUNCT ".")
(ID "match")
(PUNCT "(")
(ID "first_line")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(ID "match")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "adjust")
(PUNCT "=")
(KEYWORD True)
(NEWLINE)
(ID "post_interp")
(PUNCT "=")
(ID "match")
(PUNCT ".")
(ID "group")
(PUNCT "(")
(LIT 1)
(PUNCT ")")
(KEYWORD or)
(LIT #"")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD if)
(ID "adjust")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "log")
(PUNCT ".")
(ID "info")
(PUNCT "(")
(LIT "copying and adjusting %s -> %s")
(PUNCT ",")
(ID "script")
(PUNCT ",")
(ID "self")
(PUNCT ".")
(ID "build_dir")
(PUNCT ")")
(NEWLINE)
(ID "updated_files")
(PUNCT ".")
(ID "append")
(PUNCT "(")
(ID "outfile")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(KEYWORD not)
(ID "self")
(PUNCT ".")
(ID "dry_run")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(KEYWORD not)
(ID "sysconfig")
(PUNCT ".")
(ID "python_build")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "executable")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "executable")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "executable")
(PUNCT "=")
(ID "os")
(PUNCT ".")
(ID "path")
(PUNCT ".")
(ID "join")
(PUNCT "(")
(ID "sysconfig")
(PUNCT ".")
(ID "get_config_var")
(PUNCT "(")
(LIT "BINDIR")
(PUNCT ")")
(PUNCT ",")
(LIT "python%s%s")
(PUNCT "%")
(PUNCT "(")
(ID "sysconfig")
(PUNCT ".")
(ID "get_config_var")
(PUNCT "(")
(LIT "VERSION")
(PUNCT ")")
(PUNCT ",")
(ID "sysconfig")
(PUNCT ".")
(ID "get_config_var")
(PUNCT "(")
(LIT "EXE")
(PUNCT ")")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "executable")
(PUNCT "=")
(ID "os")
(PUNCT ".")
(ID "fsencode")
(PUNCT "(")
(ID "executable")
(PUNCT ")")
(NEWLINE)
(ID "shebang")
(PUNCT "=")
(LIT #"#!")
(PUNCT "+")
(ID "executable")
(PUNCT "+")
(ID "post_interp")
(PUNCT "+")
(LIT #"\n")
(NEWLINE)
(KEYWORD try)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "shebang")
(PUNCT ".")
(ID "decode")
(PUNCT "(")
(LIT "utf-8")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD except)
(ID "UnicodeDecodeError")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD raise)
(ID "ValueError")
(PUNCT "(")
(LIT "The shebang ({!r}) is not decodable ")
(LIT "from utf-8")
(PUNCT ".")
(ID "format")
(PUNCT "(")
(ID "shebang")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD try)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "shebang")
(PUNCT ".")
(ID "decode")
(PUNCT "(")
(ID "encoding")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD except)
(ID "UnicodeDecodeError")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD raise)
(ID "ValueError")
(PUNCT "(")
(LIT "The shebang ({!r}) is not decodable ")
(LIT "from the script encoding ({})")
(PUNCT ".")
(ID "format")
(PUNCT "(")
(ID "shebang")
(PUNCT ",")
(ID "encoding")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD with)
(ID "open")
(PUNCT "(")
(ID "outfile")
(PUNCT ",")
(LIT "wb")
(PUNCT ")")
(KEYWORD as)
(ID "outf")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "outf")
(PUNCT ".")
(ID "write")
(PUNCT "(")
(ID "shebang")
(PUNCT ")")
(NEWLINE)
(ID "outf")
(PUNCT ".")
(ID "writelines")
(PUNCT "(")
(ID "f")
(PUNCT ".")
(ID "readlines")
(PUNCT "(")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD if)
(ID "f")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "f")
(PUNCT ".")
(ID "close")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "f")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "f")
(PUNCT ".")
(ID "close")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "updated_files")
(PUNCT ".")
(ID "append")
(PUNCT "(")
(ID "outfile")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "copy_file")
(PUNCT "(")
(ID "script")
(PUNCT ",")
(ID "outfile")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD if)
(ID "os")
(PUNCT ".")
(ID "name")
(PUNCT "==")
(LIT "posix")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD for)
(ID "file")
(KEYWORD in)
(ID "outfiles")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "self")
(PUNCT ".")
(ID "dry_run")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "log")
(PUNCT ".")
(ID "info")
(PUNCT "(")
(LIT "changing mode of %s")
(PUNCT ",")
(ID "file")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "oldmode")
(PUNCT "=")
(ID "os")
(PUNCT ".")
(ID "stat")
(PUNCT "(")
(ID "file")
(PUNCT ")")
(PUNCT "[")
(ID "ST_MODE")
(PUNCT "]")
(PUNCT "&")
(LIT 4095)
(NEWLINE)
(ID "newmode")
(PUNCT "=")
(PUNCT "(")
(ID "oldmode")
(PUNCT "|")
(LIT 365)
(PUNCT ")")
(PUNCT "&")
(LIT 4095)
(NEWLINE)
(KEYWORD if)
(ID "newmode")
(PUNCT "!=")
(ID "oldmode")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "log")
(PUNCT ".")
(ID "info")
(PUNCT "(")
(LIT "changing mode of %s from %o to %o")
(PUNCT ",")
(ID "file")
(PUNCT ",")
(ID "oldmode")
(PUNCT ",")
(ID "newmode")
(PUNCT ")")
(NEWLINE)
(ID "os")
(PUNCT ".")
(ID "chmod")
(PUNCT "(")
(ID "file")
(PUNCT ",")
(ID "newmode")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(DEDENT)
(DEDENT)
(KEYWORD return)
(ID "outfiles")
(PUNCT ",")
(ID "updated_files")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD class)
(ID "build_scripts_2to3")
(PUNCT "(")
(ID "build_scripts")
(PUNCT ",")
(ID "Mixin2to3")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD def)
(ID "copy_scripts")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "outfiles")
(PUNCT ",")
(ID "updated_files")
(PUNCT "=")
(ID "build_scripts")
(PUNCT ".")
(ID "copy_scripts")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(KEYWORD not)
(ID "self")
(PUNCT ".")
(ID "dry_run")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "run_2to3")
(PUNCT "(")
(ID "updated_files")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD return)
(ID "outfiles")
(PUNCT ",")
(ID "updated_files")
(NEWLINE)
(DEDENT)
(DEDENT)
(ENDMARKER)
