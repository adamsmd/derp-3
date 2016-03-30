(LIT "\n    ast\n    ~~~\n\n    The `ast` module helps Python applications to process trees of the Python\n    abstract syntax grammar.  The abstract syntax itself might change with\n    each Python release; this module helps to find out programmatically what\n    the current grammar looks like and allows modifications of it.\n\n    An abstract syntax tree can be generated by passing `ast.PyCF_ONLY_AST` as\n    a flag to the `compile()` builtin function or by using the `parse()`\n    function from this module.  The result will be a tree of objects whose\n    classes all inherit from `ast.AST`.\n\n    A modified abstract syntax tree can be compiled into a Python code object\n    using the built-in `compile()` function.\n\n    Additionally various helper functions are provided that make working with\n    the trees simpler.  The main intention of the helper functions and this\n    module in general is to provide an easy to use interface for libraries\n    that work tightly with the python syntax (template engines for example).\n\n\n    :copyright: Copyright 2008 by Armin Ronacher.\n    :license: Python License.\n")
(NEWLINE)
(KEYWORD from)
(ID "_ast")
(KEYWORD import)
(PUNCT "*")
(NEWLINE)
(KEYWORD def)
(ID "parse")
(PUNCT "(")
(ID "source")
(PUNCT ",")
(ID "filename")
(PUNCT "=")
(LIT "<unknown>")
(PUNCT ",")
(ID "mode")
(PUNCT "=")
(LIT "exec")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "\n    Parse the source into an AST node.\n    Equivalent to compile(source, filename, mode, PyCF_ONLY_AST).\n    ")
(NEWLINE)
(KEYWORD return)
(ID "compile")
(PUNCT "(")
(ID "source")
(PUNCT ",")
(ID "filename")
(PUNCT ",")
(ID "mode")
(PUNCT ",")
(ID "PyCF_ONLY_AST")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "literal_eval")
(PUNCT "(")
(ID "node_or_string")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "\n    Safely evaluate an expression node or a string containing a Python\n    expression.  The string or node provided may only consist of the following\n    Python literal structures: strings, bytes, numbers, tuples, lists, dicts,\n    sets, booleans, and None.\n    ")
(NEWLINE)
(KEYWORD if)
(ID "isinstance")
(PUNCT "(")
(ID "node_or_string")
(PUNCT ",")
(ID "str")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "node_or_string")
(PUNCT "=")
(ID "parse")
(PUNCT "(")
(ID "node_or_string")
(PUNCT ",")
(ID "mode")
(PUNCT "=")
(LIT "eval")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD if)
(ID "isinstance")
(PUNCT "(")
(ID "node_or_string")
(PUNCT ",")
(ID "Expression")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "node_or_string")
(PUNCT "=")
(ID "node_or_string")
(PUNCT ".")
(ID "body")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "_convert")
(PUNCT "(")
(ID "node")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "isinstance")
(PUNCT "(")
(ID "node")
(PUNCT ",")
(PUNCT "(")
(ID "Str")
(PUNCT ",")
(ID "Bytes")
(PUNCT ")")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(ID "node")
(PUNCT ".")
(ID "s")
(NEWLINE)
(DEDENT)
(KEYWORD elif)
(ID "isinstance")
(PUNCT "(")
(ID "node")
(PUNCT ",")
(ID "Num")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(ID "node")
(PUNCT ".")
(ID "n")
(NEWLINE)
(DEDENT)
(KEYWORD elif)
(ID "isinstance")
(PUNCT "(")
(ID "node")
(PUNCT ",")
(ID "Tuple")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(ID "tuple")
(PUNCT "(")
(ID "map")
(PUNCT "(")
(ID "_convert")
(PUNCT ",")
(ID "node")
(PUNCT ".")
(ID "elts")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD elif)
(ID "isinstance")
(PUNCT "(")
(ID "node")
(PUNCT ",")
(ID "List")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(ID "list")
(PUNCT "(")
(ID "map")
(PUNCT "(")
(ID "_convert")
(PUNCT ",")
(ID "node")
(PUNCT ".")
(ID "elts")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD elif)
(ID "isinstance")
(PUNCT "(")
(ID "node")
(PUNCT ",")
(ID "Set")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(ID "set")
(PUNCT "(")
(ID "map")
(PUNCT "(")
(ID "_convert")
(PUNCT ",")
(ID "node")
(PUNCT ".")
(ID "elts")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD elif)
(ID "isinstance")
(PUNCT "(")
(ID "node")
(PUNCT ",")
(ID "Dict")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(ID "dict")
(PUNCT "(")
(PUNCT "(")
(ID "_convert")
(PUNCT "(")
(ID "k")
(PUNCT ")")
(PUNCT ",")
(ID "_convert")
(PUNCT "(")
(ID "v")
(PUNCT ")")
(PUNCT ")")
(KEYWORD for)
(ID "k")
(PUNCT ",")
(ID "v")
(KEYWORD in)
(ID "zip")
(PUNCT "(")
(ID "node")
(PUNCT ".")
(ID "keys")
(PUNCT ",")
(ID "node")
(PUNCT ".")
(ID "values")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD elif)
(ID "isinstance")
(PUNCT "(")
(ID "node")
(PUNCT ",")
(ID "NameConstant")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(ID "node")
(PUNCT ".")
(ID "value")
(NEWLINE)
(DEDENT)
(KEYWORD elif)
(ID "isinstance")
(PUNCT "(")
(ID "node")
(PUNCT ",")
(ID "UnaryOp")
(PUNCT ")")
(KEYWORD and)
(ID "isinstance")
(PUNCT "(")
(ID "node")
(PUNCT ".")
(ID "op")
(PUNCT ",")
(PUNCT "(")
(ID "UAdd")
(PUNCT ",")
(ID "USub")
(PUNCT ")")
(PUNCT ")")
(KEYWORD and)
(ID "isinstance")
(PUNCT "(")
(ID "node")
(PUNCT ".")
(ID "operand")
(PUNCT ",")
(PUNCT "(")
(ID "Num")
(PUNCT ",")
(ID "UnaryOp")
(PUNCT ",")
(ID "BinOp")
(PUNCT ")")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "operand")
(PUNCT "=")
(ID "_convert")
(PUNCT "(")
(ID "node")
(PUNCT ".")
(ID "operand")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(ID "isinstance")
(PUNCT "(")
(ID "node")
(PUNCT ".")
(ID "op")
(PUNCT ",")
(ID "UAdd")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(PUNCT "+")
(ID "operand")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(PUNCT "-")
(ID "operand")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD elif)
(ID "isinstance")
(PUNCT "(")
(ID "node")
(PUNCT ",")
(ID "BinOp")
(PUNCT ")")
(KEYWORD and)
(ID "isinstance")
(PUNCT "(")
(ID "node")
(PUNCT ".")
(ID "op")
(PUNCT ",")
(PUNCT "(")
(ID "Add")
(PUNCT ",")
(ID "Sub")
(PUNCT ")")
(PUNCT ")")
(KEYWORD and)
(ID "isinstance")
(PUNCT "(")
(ID "node")
(PUNCT ".")
(ID "right")
(PUNCT ",")
(PUNCT "(")
(ID "Num")
(PUNCT ",")
(ID "UnaryOp")
(PUNCT ",")
(ID "BinOp")
(PUNCT ")")
(PUNCT ")")
(KEYWORD and)
(ID "isinstance")
(PUNCT "(")
(ID "node")
(PUNCT ".")
(ID "left")
(PUNCT ",")
(PUNCT "(")
(ID "Num")
(PUNCT ",")
(ID "UnaryOp")
(PUNCT ",")
(ID "BinOp")
(PUNCT ")")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "left")
(PUNCT "=")
(ID "_convert")
(PUNCT "(")
(ID "node")
(PUNCT ".")
(ID "left")
(PUNCT ")")
(NEWLINE)
(ID "right")
(PUNCT "=")
(ID "_convert")
(PUNCT "(")
(ID "node")
(PUNCT ".")
(ID "right")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(ID "isinstance")
(PUNCT "(")
(ID "node")
(PUNCT ".")
(ID "op")
(PUNCT ",")
(ID "Add")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(ID "left")
(PUNCT "+")
(ID "right")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(ID "left")
(PUNCT "-")
(ID "right")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD raise)
(ID "ValueError")
(PUNCT "(")
(LIT "malformed node or string: ")
(PUNCT "+")
(ID "repr")
(PUNCT "(")
(ID "node")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD return)
(ID "_convert")
(PUNCT "(")
(ID "node_or_string")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "dump")
(PUNCT "(")
(ID "node")
(PUNCT ",")
(ID "annotate_fields")
(PUNCT "=")
(KEYWORD True)
(PUNCT ",")
(ID "include_attributes")
(PUNCT "=")
(KEYWORD False)
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "\n    Return a formatted dump of the tree in *node*.  This is mainly useful for\n    debugging purposes.  The returned string will show the names and the values\n    for fields.  This makes the code impossible to evaluate, so if evaluation is\n    wanted *annotate_fields* must be set to False.  Attributes such as line\n    numbers and column offsets are not dumped by default.  If this is wanted,\n    *include_attributes* can be set to True.\n    ")
(NEWLINE)
(KEYWORD def)
(ID "_format")
(PUNCT "(")
(ID "node")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "isinstance")
(PUNCT "(")
(ID "node")
(PUNCT ",")
(ID "AST")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "fields")
(PUNCT "=")
(PUNCT "[")
(PUNCT "(")
(ID "a")
(PUNCT ",")
(ID "_format")
(PUNCT "(")
(ID "b")
(PUNCT ")")
(PUNCT ")")
(KEYWORD for)
(ID "a")
(PUNCT ",")
(ID "b")
(KEYWORD in)
(ID "iter_fields")
(PUNCT "(")
(ID "node")
(PUNCT ")")
(PUNCT "]")
(NEWLINE)
(ID "rv")
(PUNCT "=")
(LIT "%s(%s")
(PUNCT "%")
(PUNCT "(")
(ID "node")
(PUNCT ".")
(ID "__class__")
(PUNCT ".")
(ID "__name__")
(PUNCT ",")
(LIT ", ")
(PUNCT ".")
(ID "join")
(PUNCT "(")
(PUNCT "(")
(LIT "%s=%s")
(PUNCT "%")
(ID "field")
(KEYWORD for)
(ID "field")
(KEYWORD in)
(ID "fields")
(PUNCT ")")
(KEYWORD if)
(ID "annotate_fields")
(KEYWORD else)
(PUNCT "(")
(ID "b")
(KEYWORD for)
(ID "a")
(PUNCT ",")
(ID "b")
(KEYWORD in)
(ID "fields")
(PUNCT ")")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(ID "include_attributes")
(KEYWORD and)
(ID "node")
(PUNCT ".")
(ID "_attributes")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "rv")
(PUNCT "+=")
(ID "fields")
(KEYWORD and)
(LIT ", ")
(KEYWORD or)
(LIT " ")
(NEWLINE)
(ID "rv")
(PUNCT "+=")
(LIT ", ")
(PUNCT ".")
(ID "join")
(PUNCT "(")
(LIT "%s=%s")
(PUNCT "%")
(PUNCT "(")
(ID "a")
(PUNCT ",")
(ID "_format")
(PUNCT "(")
(ID "getattr")
(PUNCT "(")
(ID "node")
(PUNCT ",")
(ID "a")
(PUNCT ")")
(PUNCT ")")
(PUNCT ")")
(KEYWORD for)
(ID "a")
(KEYWORD in)
(ID "node")
(PUNCT ".")
(ID "_attributes")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD return)
(ID "rv")
(PUNCT "+")
(LIT ")")
(NEWLINE)
(DEDENT)
(KEYWORD elif)
(ID "isinstance")
(PUNCT "(")
(ID "node")
(PUNCT ",")
(ID "list")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(LIT "[%s]")
(PUNCT "%")
(LIT ", ")
(PUNCT ".")
(ID "join")
(PUNCT "(")
(ID "_format")
(PUNCT "(")
(ID "x")
(PUNCT ")")
(KEYWORD for)
(ID "x")
(KEYWORD in)
(ID "node")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD return)
(ID "repr")
(PUNCT "(")
(ID "node")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD if)
(KEYWORD not)
(ID "isinstance")
(PUNCT "(")
(ID "node")
(PUNCT ",")
(ID "AST")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD raise)
(ID "TypeError")
(PUNCT "(")
(LIT "expected AST, got %r")
(PUNCT "%")
(ID "node")
(PUNCT ".")
(ID "__class__")
(PUNCT ".")
(ID "__name__")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD return)
(ID "_format")
(PUNCT "(")
(ID "node")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "copy_location")
(PUNCT "(")
(ID "new_node")
(PUNCT ",")
(ID "old_node")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "\n    Copy source location (`lineno` and `col_offset` attributes) from\n    *old_node* to *new_node* if possible, and return *new_node*.\n    ")
(NEWLINE)
(KEYWORD for)
(ID "attr")
(KEYWORD in)
(LIT "lineno")
(PUNCT ",")
(LIT "col_offset")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "attr")
(KEYWORD in)
(ID "old_node")
(PUNCT ".")
(ID "_attributes")
(KEYWORD and)
(ID "attr")
(KEYWORD in)
(ID "new_node")
(PUNCT ".")
(ID "_attributes")
(KEYWORD and)
(ID "hasattr")
(PUNCT "(")
(ID "old_node")
(PUNCT ",")
(ID "attr")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "setattr")
(PUNCT "(")
(ID "new_node")
(PUNCT ",")
(ID "attr")
(PUNCT ",")
(ID "getattr")
(PUNCT "(")
(ID "old_node")
(PUNCT ",")
(ID "attr")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD return)
(ID "new_node")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "fix_missing_locations")
(PUNCT "(")
(ID "node")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "\n    When you compile a node tree with compile(), the compiler expects lineno and\n    col_offset attributes for every node that supports them.  This is rather\n    tedious to fill in for generated nodes, so this helper adds these attributes\n    recursively where not already set, by setting them to the values of the\n    parent node.  It works recursively starting at *node*.\n    ")
(NEWLINE)
(KEYWORD def)
(ID "_fix")
(PUNCT "(")
(ID "node")
(PUNCT ",")
(ID "lineno")
(PUNCT ",")
(ID "col_offset")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(LIT "lineno")
(KEYWORD in)
(ID "node")
(PUNCT ".")
(ID "_attributes")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(KEYWORD not)
(ID "hasattr")
(PUNCT "(")
(ID "node")
(PUNCT ",")
(LIT "lineno")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "node")
(PUNCT ".")
(ID "lineno")
(PUNCT "=")
(ID "lineno")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "lineno")
(PUNCT "=")
(ID "node")
(PUNCT ".")
(ID "lineno")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD if)
(LIT "col_offset")
(KEYWORD in)
(ID "node")
(PUNCT ".")
(ID "_attributes")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(KEYWORD not)
(ID "hasattr")
(PUNCT "(")
(ID "node")
(PUNCT ",")
(LIT "col_offset")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "node")
(PUNCT ".")
(ID "col_offset")
(PUNCT "=")
(ID "col_offset")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "col_offset")
(PUNCT "=")
(ID "node")
(PUNCT ".")
(ID "col_offset")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD for)
(ID "child")
(KEYWORD in)
(ID "iter_child_nodes")
(PUNCT "(")
(ID "node")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "_fix")
(PUNCT "(")
(ID "child")
(PUNCT ",")
(ID "lineno")
(PUNCT ",")
(ID "col_offset")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(ID "_fix")
(PUNCT "(")
(ID "node")
(PUNCT ",")
(LIT 1)
(PUNCT ",")
(LIT 0)
(PUNCT ")")
(NEWLINE)
(KEYWORD return)
(ID "node")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "increment_lineno")
(PUNCT "(")
(ID "node")
(PUNCT ",")
(ID "n")
(PUNCT "=")
(LIT 1)
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "\n    Increment the line number of each node in the tree starting at *node* by *n*.\n    This is useful to \"move code\" to a different location in a file.\n    ")
(NEWLINE)
(KEYWORD for)
(ID "child")
(KEYWORD in)
(ID "walk")
(PUNCT "(")
(ID "node")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(LIT "lineno")
(KEYWORD in)
(ID "child")
(PUNCT ".")
(ID "_attributes")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "child")
(PUNCT ".")
(ID "lineno")
(PUNCT "=")
(ID "getattr")
(PUNCT "(")
(ID "child")
(PUNCT ",")
(LIT "lineno")
(PUNCT ",")
(LIT 0)
(PUNCT ")")
(PUNCT "+")
(ID "n")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD return)
(ID "node")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "iter_fields")
(PUNCT "(")
(ID "node")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "\n    Yield a tuple of ``(fieldname, value)`` for each field in ``node._fields``\n    that is present on *node*.\n    ")
(NEWLINE)
(KEYWORD for)
(ID "field")
(KEYWORD in)
(ID "node")
(PUNCT ".")
(ID "_fields")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD try)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD yield)
(ID "field")
(PUNCT ",")
(ID "getattr")
(PUNCT "(")
(ID "node")
(PUNCT ",")
(ID "field")
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
(DEDENT)
(KEYWORD def)
(ID "iter_child_nodes")
(PUNCT "(")
(ID "node")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "\n    Yield all direct child nodes of *node*, that is, all fields that are nodes\n    and all items of fields that are lists of nodes.\n    ")
(NEWLINE)
(KEYWORD for)
(ID "name")
(PUNCT ",")
(ID "field")
(KEYWORD in)
(ID "iter_fields")
(PUNCT "(")
(ID "node")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "isinstance")
(PUNCT "(")
(ID "field")
(PUNCT ",")
(ID "AST")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD yield)
(ID "field")
(NEWLINE)
(DEDENT)
(KEYWORD elif)
(ID "isinstance")
(PUNCT "(")
(ID "field")
(PUNCT ",")
(ID "list")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD for)
(ID "item")
(KEYWORD in)
(ID "field")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "isinstance")
(PUNCT "(")
(ID "item")
(PUNCT ",")
(ID "AST")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD yield)
(ID "item")
(NEWLINE)
(DEDENT)
(DEDENT)
(DEDENT)
(DEDENT)
(DEDENT)
(KEYWORD def)
(ID "get_docstring")
(PUNCT "(")
(ID "node")
(PUNCT ",")
(ID "clean")
(PUNCT "=")
(KEYWORD True)
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "\n    Return the docstring for the given node or None if no docstring can\n    be found.  If the node provided does not have docstrings a TypeError\n    will be raised.\n    ")
(NEWLINE)
(KEYWORD if)
(KEYWORD not)
(ID "isinstance")
(PUNCT "(")
(ID "node")
(PUNCT ",")
(PUNCT "(")
(ID "FunctionDef")
(PUNCT ",")
(ID "ClassDef")
(PUNCT ",")
(ID "Module")
(PUNCT ")")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD raise)
(ID "TypeError")
(PUNCT "(")
(LIT "%r can't have docstrings")
(PUNCT "%")
(ID "node")
(PUNCT ".")
(ID "__class__")
(PUNCT ".")
(ID "__name__")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD if)
(ID "node")
(PUNCT ".")
(ID "body")
(KEYWORD and)
(ID "isinstance")
(PUNCT "(")
(ID "node")
(PUNCT ".")
(ID "body")
(PUNCT "[")
(LIT 0)
(PUNCT "]")
(PUNCT ",")
(ID "Expr")
(PUNCT ")")
(KEYWORD and)
(ID "isinstance")
(PUNCT "(")
(ID "node")
(PUNCT ".")
(ID "body")
(PUNCT "[")
(LIT 0)
(PUNCT "]")
(PUNCT ".")
(ID "value")
(PUNCT ",")
(ID "Str")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "clean")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD import)
(ID "inspect")
(NEWLINE)
(KEYWORD return)
(ID "inspect")
(PUNCT ".")
(ID "cleandoc")
(PUNCT "(")
(ID "node")
(PUNCT ".")
(ID "body")
(PUNCT "[")
(LIT 0)
(PUNCT "]")
(PUNCT ".")
(ID "value")
(PUNCT ".")
(ID "s")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD return)
(ID "node")
(PUNCT ".")
(ID "body")
(PUNCT "[")
(LIT 0)
(PUNCT "]")
(PUNCT ".")
(ID "value")
(PUNCT ".")
(ID "s")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD def)
(ID "walk")
(PUNCT "(")
(ID "node")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "\n    Recursively yield all descendant nodes in the tree starting at *node*\n    (including *node* itself), in no specified order.  This is useful if you\n    only want to modify nodes in place and don't care about the context.\n    ")
(NEWLINE)
(KEYWORD from)
(ID "collections")
(KEYWORD import)
(ID "deque")
(NEWLINE)
(ID "todo")
(PUNCT "=")
(ID "deque")
(PUNCT "(")
(PUNCT "[")
(ID "node")
(PUNCT "]")
(PUNCT ")")
(NEWLINE)
(KEYWORD while)
(ID "todo")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "node")
(PUNCT "=")
(ID "todo")
(PUNCT ".")
(ID "popleft")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "todo")
(PUNCT ".")
(ID "extend")
(PUNCT "(")
(ID "iter_child_nodes")
(PUNCT "(")
(ID "node")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(KEYWORD yield)
(ID "node")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD class)
(ID "NodeVisitor")
(PUNCT "(")
(ID "object")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "\n    A node visitor base class that walks the abstract syntax tree and calls a\n    visitor function for every node found.  This function may return a value\n    which is forwarded by the `visit` method.\n\n    This class is meant to be subclassed, with the subclass adding visitor\n    methods.\n\n    Per default the visitor functions for the nodes are ``'visit_'`` +\n    class name of the node.  So a `TryFinally` node visit function would\n    be `visit_TryFinally`.  This behavior can be changed by overriding\n    the `visit` method.  If no visitor function exists for a node\n    (return value `None`) the `generic_visit` visitor is used instead.\n\n    Don't use the `NodeVisitor` if you want to apply changes to nodes during\n    traversing.  For this a special visitor exists (`NodeTransformer`) that\n    allows modifications.\n    ")
(NEWLINE)
(KEYWORD def)
(ID "visit")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "node")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Visit a node.")
(NEWLINE)
(ID "method")
(PUNCT "=")
(LIT "visit_")
(PUNCT "+")
(ID "node")
(PUNCT ".")
(ID "__class__")
(PUNCT ".")
(ID "__name__")
(NEWLINE)
(ID "visitor")
(PUNCT "=")
(ID "getattr")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "method")
(PUNCT ",")
(ID "self")
(PUNCT ".")
(ID "generic_visit")
(PUNCT ")")
(NEWLINE)
(KEYWORD return)
(ID "visitor")
(PUNCT "(")
(ID "node")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "generic_visit")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "node")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Called if no explicit visitor function exists for a node.")
(NEWLINE)
(KEYWORD for)
(ID "field")
(PUNCT ",")
(ID "value")
(KEYWORD in)
(ID "iter_fields")
(PUNCT "(")
(ID "node")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "isinstance")
(PUNCT "(")
(ID "value")
(PUNCT ",")
(ID "list")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD for)
(ID "item")
(KEYWORD in)
(ID "value")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "isinstance")
(PUNCT "(")
(ID "item")
(PUNCT ",")
(ID "AST")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "visit")
(PUNCT "(")
(ID "item")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(DEDENT)
(KEYWORD elif)
(ID "isinstance")
(PUNCT "(")
(ID "value")
(PUNCT ",")
(ID "AST")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "visit")
(PUNCT "(")
(ID "value")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(DEDENT)
(DEDENT)
(KEYWORD class)
(ID "NodeTransformer")
(PUNCT "(")
(ID "NodeVisitor")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "\n    A :class:`NodeVisitor` subclass that walks the abstract syntax tree and\n    allows modification of nodes.\n\n    The `NodeTransformer` will walk the AST and use the return value of the\n    visitor methods to replace or remove the old node.  If the return value of\n    the visitor method is ``None``, the node will be removed from its location,\n    otherwise it is replaced with the return value.  The return value may be the\n    original node in which case no replacement takes place.\n\n    Here is an example transformer that rewrites all occurrences of name lookups\n    (``foo``) to ``data['foo']``::\n\n       class RewriteName(NodeTransformer):\n\n           def visit_Name(self, node):\n               return copy_location(Subscript(\n                   value=Name(id='data', ctx=Load()),\n                   slice=Index(value=Str(s=node.id)),\n                   ctx=node.ctx\n               ), node)\n\n    Keep in mind that if the node you're operating on has child nodes you must\n    either transform the child nodes yourself or call the :meth:`generic_visit`\n    method for the node first.\n\n    For nodes that were part of a collection of statements (that applies to all\n    statement nodes), the visitor may also return a list of nodes rather than\n    just a single node.\n\n    Usually you use the transformer like this::\n\n       node = YourTransformer().visit(node)\n    ")
(NEWLINE)
(KEYWORD def)
(ID "generic_visit")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "node")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD for)
(ID "field")
(PUNCT ",")
(ID "old_value")
(KEYWORD in)
(ID "iter_fields")
(PUNCT "(")
(ID "node")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "old_value")
(PUNCT "=")
(ID "getattr")
(PUNCT "(")
(ID "node")
(PUNCT ",")
(ID "field")
(PUNCT ",")
(KEYWORD None)
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(ID "isinstance")
(PUNCT "(")
(ID "old_value")
(PUNCT ",")
(ID "list")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "new_values")
(PUNCT "=")
(PUNCT "[")
(PUNCT "]")
(NEWLINE)
(KEYWORD for)
(ID "value")
(KEYWORD in)
(ID "old_value")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "isinstance")
(PUNCT "(")
(ID "value")
(PUNCT ",")
(ID "AST")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "value")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "visit")
(PUNCT "(")
(ID "value")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(ID "value")
(KEYWORD is)
(KEYWORD None)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD continue)
(NEWLINE)
(DEDENT)
(KEYWORD elif)
(KEYWORD not)
(ID "isinstance")
(PUNCT "(")
(ID "value")
(PUNCT ",")
(ID "AST")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "new_values")
(PUNCT ".")
(ID "extend")
(PUNCT "(")
(ID "value")
(PUNCT ")")
(NEWLINE)
(KEYWORD continue)
(NEWLINE)
(DEDENT)
(DEDENT)
(ID "new_values")
(PUNCT ".")
(ID "append")
(PUNCT "(")
(ID "value")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "old_value")
(PUNCT "[")
(PUNCT ":")
(PUNCT "]")
(PUNCT "=")
(ID "new_values")
(NEWLINE)
(DEDENT)
(KEYWORD elif)
(ID "isinstance")
(PUNCT "(")
(ID "old_value")
(PUNCT ",")
(ID "AST")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "new_node")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "visit")
(PUNCT "(")
(ID "old_value")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(ID "new_node")
(KEYWORD is)
(KEYWORD None)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "delattr")
(PUNCT "(")
(ID "node")
(PUNCT ",")
(ID "field")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "setattr")
(PUNCT "(")
(ID "node")
(PUNCT ",")
(ID "field")
(PUNCT ",")
(ID "new_node")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(DEDENT)
(KEYWORD return)
(ID "node")
(NEWLINE)
(DEDENT)
(DEDENT)
(ENDMARKER)
