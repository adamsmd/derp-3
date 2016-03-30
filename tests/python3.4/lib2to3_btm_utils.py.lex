(LIT "Utility functions used by the btm_matcher module")
(NEWLINE)
(KEYWORD from)
(PUNCT ".")
(KEYWORD import)
(ID "pytree")
(NEWLINE)
(KEYWORD from)
(PUNCT ".")
(ID "pgen2")
(KEYWORD import)
(ID "grammar")
(PUNCT ",")
(ID "token")
(NEWLINE)
(KEYWORD from)
(PUNCT ".")
(ID "pygram")
(KEYWORD import)
(ID "pattern_symbols")
(PUNCT ",")
(ID "python_symbols")
(NEWLINE)
(ID "syms")
(PUNCT "=")
(ID "pattern_symbols")
(NEWLINE)
(ID "pysyms")
(PUNCT "=")
(ID "python_symbols")
(NEWLINE)
(ID "tokens")
(PUNCT "=")
(ID "grammar")
(PUNCT ".")
(ID "opmap")
(NEWLINE)
(ID "token_labels")
(PUNCT "=")
(ID "token")
(NEWLINE)
(ID "TYPE_ANY")
(PUNCT "=")
(PUNCT "-")
(LIT 1)
(NEWLINE)
(ID "TYPE_ALTERNATIVES")
(PUNCT "=")
(PUNCT "-")
(LIT 2)
(NEWLINE)
(ID "TYPE_GROUP")
(PUNCT "=")
(PUNCT "-")
(LIT 3)
(NEWLINE)
(KEYWORD class)
(ID "MinNode")
(PUNCT "(")
(ID "object")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "This class serves as an intermediate representation of the\n    pattern tree during the conversion to sets of leaf-to-root\n    subpatterns")
(NEWLINE)
(KEYWORD def)
(ID "__init__")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "type")
(PUNCT "=")
(KEYWORD None)
(PUNCT ",")
(ID "name")
(PUNCT "=")
(KEYWORD None)
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "type")
(PUNCT "=")
(ID "type")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "name")
(PUNCT "=")
(ID "name")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "children")
(PUNCT "=")
(PUNCT "[")
(PUNCT "]")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "leaf")
(PUNCT "=")
(KEYWORD False)
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "parent")
(PUNCT "=")
(KEYWORD None)
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "alternatives")
(PUNCT "=")
(PUNCT "[")
(PUNCT "]")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "group")
(PUNCT "=")
(PUNCT "[")
(PUNCT "]")
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
(ID "str")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "type")
(PUNCT ")")
(PUNCT "+")
(LIT " ")
(PUNCT "+")
(ID "str")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "name")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "leaf_to_root")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Internal method. Returns a characteristic path of the\n        pattern tree. This method must be run for all leaves until the\n        linear subpatterns are merged into a single")
(NEWLINE)
(ID "node")
(PUNCT "=")
(ID "self")
(NEWLINE)
(ID "subp")
(PUNCT "=")
(PUNCT "[")
(PUNCT "]")
(NEWLINE)
(KEYWORD while)
(ID "node")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "node")
(PUNCT ".")
(ID "type")
(PUNCT "==")
(ID "TYPE_ALTERNATIVES")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "node")
(PUNCT ".")
(ID "alternatives")
(PUNCT ".")
(ID "append")
(PUNCT "(")
(ID "subp")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(ID "len")
(PUNCT "(")
(ID "node")
(PUNCT ".")
(ID "alternatives")
(PUNCT ")")
(PUNCT "==")
(ID "len")
(PUNCT "(")
(ID "node")
(PUNCT ".")
(ID "children")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "subp")
(PUNCT "=")
(PUNCT "[")
(ID "tuple")
(PUNCT "(")
(ID "node")
(PUNCT ".")
(ID "alternatives")
(PUNCT ")")
(PUNCT "]")
(NEWLINE)
(ID "node")
(PUNCT ".")
(ID "alternatives")
(PUNCT "=")
(PUNCT "[")
(PUNCT "]")
(NEWLINE)
(ID "node")
(PUNCT "=")
(ID "node")
(PUNCT ".")
(ID "parent")
(NEWLINE)
(KEYWORD continue)
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "node")
(PUNCT "=")
(ID "node")
(PUNCT ".")
(ID "parent")
(NEWLINE)
(ID "subp")
(PUNCT "=")
(KEYWORD None)
(NEWLINE)
(KEYWORD break)
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD if)
(ID "node")
(PUNCT ".")
(ID "type")
(PUNCT "==")
(ID "TYPE_GROUP")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "node")
(PUNCT ".")
(ID "group")
(PUNCT ".")
(ID "append")
(PUNCT "(")
(ID "subp")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(ID "len")
(PUNCT "(")
(ID "node")
(PUNCT ".")
(ID "group")
(PUNCT ")")
(PUNCT "==")
(ID "len")
(PUNCT "(")
(ID "node")
(PUNCT ".")
(ID "children")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "subp")
(PUNCT "=")
(ID "get_characteristic_subpattern")
(PUNCT "(")
(ID "node")
(PUNCT ".")
(ID "group")
(PUNCT ")")
(NEWLINE)
(ID "node")
(PUNCT ".")
(ID "group")
(PUNCT "=")
(PUNCT "[")
(PUNCT "]")
(NEWLINE)
(ID "node")
(PUNCT "=")
(ID "node")
(PUNCT ".")
(ID "parent")
(NEWLINE)
(KEYWORD continue)
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "node")
(PUNCT "=")
(ID "node")
(PUNCT ".")
(ID "parent")
(NEWLINE)
(ID "subp")
(PUNCT "=")
(KEYWORD None)
(NEWLINE)
(KEYWORD break)
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD if)
(ID "node")
(PUNCT ".")
(ID "type")
(PUNCT "==")
(ID "token_labels")
(PUNCT ".")
(ID "NAME")
(KEYWORD and)
(ID "node")
(PUNCT ".")
(ID "name")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "subp")
(PUNCT ".")
(ID "append")
(PUNCT "(")
(ID "node")
(PUNCT ".")
(ID "name")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "subp")
(PUNCT ".")
(ID "append")
(PUNCT "(")
(ID "node")
(PUNCT ".")
(ID "type")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "node")
(PUNCT "=")
(ID "node")
(PUNCT ".")
(ID "parent")
(NEWLINE)
(DEDENT)
(KEYWORD return)
(ID "subp")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "get_linear_subpattern")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Drives the leaf_to_root method. The reason that\n        leaf_to_root must be run multiple times is because we need to\n        reject 'group' matches; for example the alternative form\n        (a | b c) creates a group [b c] that needs to be matched. Since\n        matching multiple linear patterns overcomes the automaton's\n        capabilities, leaf_to_root merges each group into a single\n        choice based on 'characteristic'ity,\n\n        i.e. (a|b c) -> (a|b) if b more characteristic than c\n\n        Returns: The most 'characteristic'(as defined by\n          get_characteristic_subpattern) path for the compiled pattern\n          tree.\n        ")
(NEWLINE)
(KEYWORD for)
(ID "l")
(KEYWORD in)
(ID "self")
(PUNCT ".")
(ID "leaves")
(PUNCT "(")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "subp")
(PUNCT "=")
(ID "l")
(PUNCT ".")
(ID "leaf_to_root")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(ID "subp")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(ID "subp")
(NEWLINE)
(DEDENT)
(DEDENT)
(DEDENT)
(KEYWORD def)
(ID "leaves")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Generator that returns the leaves of the tree")
(NEWLINE)
(KEYWORD for)
(ID "child")
(KEYWORD in)
(ID "self")
(PUNCT ".")
(ID "children")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD yield)
(KEYWORD from)
(ID "child")
(PUNCT ".")
(ID "leaves")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD if)
(KEYWORD not)
(ID "self")
(PUNCT ".")
(ID "children")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD yield)
(ID "self")
(NEWLINE)
(DEDENT)
(DEDENT)
(DEDENT)
(KEYWORD def)
(ID "reduce_tree")
(PUNCT "(")
(ID "node")
(PUNCT ",")
(ID "parent")
(PUNCT "=")
(KEYWORD None)
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "\n    Internal function. Reduces a compiled pattern tree to an\n    intermediate representation suitable for feeding the\n    automaton. This also trims off any optional pattern elements(like\n    [a], a*).\n    ")
(NEWLINE)
(ID "new_node")
(PUNCT "=")
(KEYWORD None)
(NEWLINE)
(KEYWORD if)
(ID "node")
(PUNCT ".")
(ID "type")
(PUNCT "==")
(ID "syms")
(PUNCT ".")
(ID "Matcher")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "node")
(PUNCT "=")
(ID "node")
(PUNCT ".")
(ID "children")
(PUNCT "[")
(LIT 0)
(PUNCT "]")
(NEWLINE)
(DEDENT)
(KEYWORD if)
(ID "node")
(PUNCT ".")
(ID "type")
(PUNCT "==")
(ID "syms")
(PUNCT ".")
(ID "Alternatives")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "len")
(PUNCT "(")
(ID "node")
(PUNCT ".")
(ID "children")
(PUNCT ")")
(PUNCT "<=")
(LIT 2)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "new_node")
(PUNCT "=")
(ID "reduce_tree")
(PUNCT "(")
(ID "node")
(PUNCT ".")
(ID "children")
(PUNCT "[")
(LIT 0)
(PUNCT "]")
(PUNCT ",")
(ID "parent")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "new_node")
(PUNCT "=")
(ID "MinNode")
(PUNCT "(")
(ID "type")
(PUNCT "=")
(ID "TYPE_ALTERNATIVES")
(PUNCT ")")
(NEWLINE)
(KEYWORD for)
(ID "child")
(KEYWORD in)
(ID "node")
(PUNCT ".")
(ID "children")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "node")
(PUNCT ".")
(ID "children")
(PUNCT ".")
(ID "index")
(PUNCT "(")
(ID "child")
(PUNCT ")")
(PUNCT "%")
(LIT 2)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD continue)
(NEWLINE)
(DEDENT)
(ID "reduced")
(PUNCT "=")
(ID "reduce_tree")
(PUNCT "(")
(ID "child")
(PUNCT ",")
(ID "new_node")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(ID "reduced")
(KEYWORD is)
(KEYWORD not)
(KEYWORD None)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "new_node")
(PUNCT ".")
(ID "children")
(PUNCT ".")
(ID "append")
(PUNCT "(")
(ID "reduced")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(DEDENT)
(DEDENT)
(KEYWORD elif)
(ID "node")
(PUNCT ".")
(ID "type")
(PUNCT "==")
(ID "syms")
(PUNCT ".")
(ID "Alternative")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "len")
(PUNCT "(")
(ID "node")
(PUNCT ".")
(ID "children")
(PUNCT ")")
(PUNCT ">")
(LIT 1)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "new_node")
(PUNCT "=")
(ID "MinNode")
(PUNCT "(")
(ID "type")
(PUNCT "=")
(ID "TYPE_GROUP")
(PUNCT ")")
(NEWLINE)
(KEYWORD for)
(ID "child")
(KEYWORD in)
(ID "node")
(PUNCT ".")
(ID "children")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "reduced")
(PUNCT "=")
(ID "reduce_tree")
(PUNCT "(")
(ID "child")
(PUNCT ",")
(ID "new_node")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(ID "reduced")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "new_node")
(PUNCT ".")
(ID "children")
(PUNCT ".")
(ID "append")
(PUNCT "(")
(ID "reduced")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD if)
(KEYWORD not)
(ID "new_node")
(PUNCT ".")
(ID "children")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "new_node")
(PUNCT "=")
(KEYWORD None)
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "new_node")
(PUNCT "=")
(ID "reduce_tree")
(PUNCT "(")
(ID "node")
(PUNCT ".")
(ID "children")
(PUNCT "[")
(LIT 0)
(PUNCT "]")
(PUNCT ",")
(ID "parent")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD elif)
(ID "node")
(PUNCT ".")
(ID "type")
(PUNCT "==")
(ID "syms")
(PUNCT ".")
(ID "Unit")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(PUNCT "(")
(ID "isinstance")
(PUNCT "(")
(ID "node")
(PUNCT ".")
(ID "children")
(PUNCT "[")
(LIT 0)
(PUNCT "]")
(PUNCT ",")
(ID "pytree")
(PUNCT ".")
(ID "Leaf")
(PUNCT ")")
(KEYWORD and)
(ID "node")
(PUNCT ".")
(ID "children")
(PUNCT "[")
(LIT 0)
(PUNCT "]")
(PUNCT ".")
(ID "value")
(PUNCT "==")
(LIT "(")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(ID "reduce_tree")
(PUNCT "(")
(ID "node")
(PUNCT ".")
(ID "children")
(PUNCT "[")
(LIT 1)
(PUNCT "]")
(PUNCT ",")
(ID "parent")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD if)
(PUNCT "(")
(PUNCT "(")
(ID "isinstance")
(PUNCT "(")
(ID "node")
(PUNCT ".")
(ID "children")
(PUNCT "[")
(LIT 0)
(PUNCT "]")
(PUNCT ",")
(ID "pytree")
(PUNCT ".")
(ID "Leaf")
(PUNCT ")")
(KEYWORD and)
(ID "node")
(PUNCT ".")
(ID "children")
(PUNCT "[")
(LIT 0)
(PUNCT "]")
(PUNCT ".")
(ID "value")
(PUNCT "==")
(LIT "[")
(PUNCT ")")
(KEYWORD or)
(PUNCT "(")
(ID "len")
(PUNCT "(")
(ID "node")
(PUNCT ".")
(ID "children")
(PUNCT ")")
(PUNCT ">")
(LIT 1)
(KEYWORD and)
(ID "hasattr")
(PUNCT "(")
(ID "node")
(PUNCT ".")
(ID "children")
(PUNCT "[")
(LIT 1)
(PUNCT "]")
(PUNCT ",")
(LIT "value")
(PUNCT ")")
(KEYWORD and)
(ID "node")
(PUNCT ".")
(ID "children")
(PUNCT "[")
(LIT 1)
(PUNCT "]")
(PUNCT ".")
(ID "value")
(PUNCT "==")
(LIT "[")
(PUNCT ")")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(KEYWORD None)
(NEWLINE)
(DEDENT)
(ID "leaf")
(PUNCT "=")
(KEYWORD True)
(NEWLINE)
(ID "details_node")
(PUNCT "=")
(KEYWORD None)
(NEWLINE)
(ID "alternatives_node")
(PUNCT "=")
(KEYWORD None)
(NEWLINE)
(ID "has_repeater")
(PUNCT "=")
(KEYWORD False)
(NEWLINE)
(ID "repeater_node")
(PUNCT "=")
(KEYWORD None)
(NEWLINE)
(ID "has_variable_name")
(PUNCT "=")
(KEYWORD False)
(NEWLINE)
(KEYWORD for)
(ID "child")
(KEYWORD in)
(ID "node")
(PUNCT ".")
(ID "children")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "child")
(PUNCT ".")
(ID "type")
(PUNCT "==")
(ID "syms")
(PUNCT ".")
(ID "Details")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "leaf")
(PUNCT "=")
(KEYWORD False)
(NEWLINE)
(ID "details_node")
(PUNCT "=")
(ID "child")
(NEWLINE)
(DEDENT)
(KEYWORD elif)
(ID "child")
(PUNCT ".")
(ID "type")
(PUNCT "==")
(ID "syms")
(PUNCT ".")
(ID "Repeater")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "has_repeater")
(PUNCT "=")
(KEYWORD True)
(NEWLINE)
(ID "repeater_node")
(PUNCT "=")
(ID "child")
(NEWLINE)
(DEDENT)
(KEYWORD elif)
(ID "child")
(PUNCT ".")
(ID "type")
(PUNCT "==")
(ID "syms")
(PUNCT ".")
(ID "Alternatives")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "alternatives_node")
(PUNCT "=")
(ID "child")
(NEWLINE)
(DEDENT)
(KEYWORD if)
(ID "hasattr")
(PUNCT "(")
(ID "child")
(PUNCT ",")
(LIT "value")
(PUNCT ")")
(KEYWORD and)
(ID "child")
(PUNCT ".")
(ID "value")
(PUNCT "==")
(LIT "=")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "has_variable_name")
(PUNCT "=")
(KEYWORD True)
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD if)
(ID "has_variable_name")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "name_leaf")
(PUNCT "=")
(ID "node")
(PUNCT ".")
(ID "children")
(PUNCT "[")
(LIT 2)
(PUNCT "]")
(NEWLINE)
(KEYWORD if)
(ID "hasattr")
(PUNCT "(")
(ID "name_leaf")
(PUNCT ",")
(LIT "value")
(PUNCT ")")
(KEYWORD and)
(ID "name_leaf")
(PUNCT ".")
(ID "value")
(PUNCT "==")
(LIT "(")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "name_leaf")
(PUNCT "=")
(ID "node")
(PUNCT ".")
(ID "children")
(PUNCT "[")
(LIT 3)
(PUNCT "]")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "name_leaf")
(PUNCT "=")
(ID "node")
(PUNCT ".")
(ID "children")
(PUNCT "[")
(LIT 0)
(PUNCT "]")
(NEWLINE)
(DEDENT)
(KEYWORD if)
(ID "name_leaf")
(PUNCT ".")
(ID "type")
(PUNCT "==")
(ID "token_labels")
(PUNCT ".")
(ID "NAME")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "name_leaf")
(PUNCT ".")
(ID "value")
(PUNCT "==")
(LIT "any")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "new_node")
(PUNCT "=")
(ID "MinNode")
(PUNCT "(")
(ID "type")
(PUNCT "=")
(ID "TYPE_ANY")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "hasattr")
(PUNCT "(")
(ID "token_labels")
(PUNCT ",")
(ID "name_leaf")
(PUNCT ".")
(ID "value")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "new_node")
(PUNCT "=")
(ID "MinNode")
(PUNCT "(")
(ID "type")
(PUNCT "=")
(ID "getattr")
(PUNCT "(")
(ID "token_labels")
(PUNCT ",")
(ID "name_leaf")
(PUNCT ".")
(ID "value")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "new_node")
(PUNCT "=")
(ID "MinNode")
(PUNCT "(")
(ID "type")
(PUNCT "=")
(ID "getattr")
(PUNCT "(")
(ID "pysyms")
(PUNCT ",")
(ID "name_leaf")
(PUNCT ".")
(ID "value")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(DEDENT)
(KEYWORD elif)
(ID "name_leaf")
(PUNCT ".")
(ID "type")
(PUNCT "==")
(ID "token_labels")
(PUNCT ".")
(ID "STRING")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "name")
(PUNCT "=")
(ID "name_leaf")
(PUNCT ".")
(ID "value")
(PUNCT ".")
(ID "strip")
(PUNCT "(")
(LIT "'")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(ID "name")
(KEYWORD in)
(ID "tokens")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "new_node")
(PUNCT "=")
(ID "MinNode")
(PUNCT "(")
(ID "type")
(PUNCT "=")
(ID "tokens")
(PUNCT "[")
(ID "name")
(PUNCT "]")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "new_node")
(PUNCT "=")
(ID "MinNode")
(PUNCT "(")
(ID "type")
(PUNCT "=")
(ID "token_labels")
(PUNCT ".")
(ID "NAME")
(PUNCT ",")
(ID "name")
(PUNCT "=")
(ID "name")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD elif)
(ID "name_leaf")
(PUNCT ".")
(ID "type")
(PUNCT "==")
(ID "syms")
(PUNCT ".")
(ID "Alternatives")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "new_node")
(PUNCT "=")
(ID "reduce_tree")
(PUNCT "(")
(ID "alternatives_node")
(PUNCT ",")
(ID "parent")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD if)
(ID "has_repeater")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "repeater_node")
(PUNCT ".")
(ID "children")
(PUNCT "[")
(LIT 0)
(PUNCT "]")
(PUNCT ".")
(ID "value")
(PUNCT "==")
(LIT "*")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "new_node")
(PUNCT "=")
(KEYWORD None)
(NEWLINE)
(DEDENT)
(KEYWORD elif)
(ID "repeater_node")
(PUNCT ".")
(ID "children")
(PUNCT "[")
(LIT 0)
(PUNCT "]")
(PUNCT ".")
(ID "value")
(PUNCT "==")
(LIT "+")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD pass)
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD raise)
(ID "NotImplementedError")
(NEWLINE)
(KEYWORD pass)
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD if)
(ID "details_node")
(KEYWORD and)
(ID "new_node")
(KEYWORD is)
(KEYWORD not)
(KEYWORD None)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD for)
(ID "child")
(KEYWORD in)
(ID "details_node")
(PUNCT ".")
(ID "children")
(PUNCT "[")
(LIT 1)
(PUNCT ":")
(PUNCT "-")
(LIT 1)
(PUNCT "]")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "reduced")
(PUNCT "=")
(ID "reduce_tree")
(PUNCT "(")
(ID "child")
(PUNCT ",")
(ID "new_node")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(ID "reduced")
(KEYWORD is)
(KEYWORD not)
(KEYWORD None)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "new_node")
(PUNCT ".")
(ID "children")
(PUNCT ".")
(ID "append")
(PUNCT "(")
(ID "reduced")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(DEDENT)
(DEDENT)
(KEYWORD if)
(ID "new_node")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "new_node")
(PUNCT ".")
(ID "parent")
(PUNCT "=")
(ID "parent")
(NEWLINE)
(DEDENT)
(KEYWORD return)
(ID "new_node")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "get_characteristic_subpattern")
(PUNCT "(")
(ID "subpatterns")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Picks the most characteristic from a list of linear patterns\n    Current order used is:\n    names > common_names > common_chars\n    ")
(NEWLINE)
(KEYWORD if)
(KEYWORD not)
(ID "isinstance")
(PUNCT "(")
(ID "subpatterns")
(PUNCT ",")
(ID "list")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(ID "subpatterns")
(NEWLINE)
(DEDENT)
(KEYWORD if)
(ID "len")
(PUNCT "(")
(ID "subpatterns")
(PUNCT ")")
(PUNCT "==")
(LIT 1)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(ID "subpatterns")
(PUNCT "[")
(LIT 0)
(PUNCT "]")
(NEWLINE)
(DEDENT)
(ID "subpatterns_with_names")
(PUNCT "=")
(PUNCT "[")
(PUNCT "]")
(NEWLINE)
(ID "subpatterns_with_common_names")
(PUNCT "=")
(PUNCT "[")
(PUNCT "]")
(NEWLINE)
(ID "common_names")
(PUNCT "=")
(PUNCT "[")
(LIT "in")
(PUNCT ",")
(LIT "for")
(PUNCT ",")
(LIT "if")
(PUNCT ",")
(LIT "not")
(PUNCT ",")
(LIT "None")
(PUNCT "]")
(NEWLINE)
(ID "subpatterns_with_common_chars")
(PUNCT "=")
(PUNCT "[")
(PUNCT "]")
(NEWLINE)
(ID "common_chars")
(PUNCT "=")
(LIT "[]().,:")
(NEWLINE)
(KEYWORD for)
(ID "subpattern")
(KEYWORD in)
(ID "subpatterns")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "any")
(PUNCT "(")
(ID "rec_test")
(PUNCT "(")
(ID "subpattern")
(PUNCT ",")
(KEYWORD lambda)
(ID "x")
(PUNCT ":")
(ID "type")
(PUNCT "(")
(ID "x")
(PUNCT ")")
(KEYWORD is)
(ID "str")
(PUNCT ")")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "any")
(PUNCT "(")
(ID "rec_test")
(PUNCT "(")
(ID "subpattern")
(PUNCT ",")
(KEYWORD lambda)
(ID "x")
(PUNCT ":")
(ID "isinstance")
(PUNCT "(")
(ID "x")
(PUNCT ",")
(ID "str")
(PUNCT ")")
(KEYWORD and)
(ID "x")
(KEYWORD in)
(ID "common_chars")
(PUNCT ")")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "subpatterns_with_common_chars")
(PUNCT ".")
(ID "append")
(PUNCT "(")
(ID "subpattern")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD elif)
(ID "any")
(PUNCT "(")
(ID "rec_test")
(PUNCT "(")
(ID "subpattern")
(PUNCT ",")
(KEYWORD lambda)
(ID "x")
(PUNCT ":")
(ID "isinstance")
(PUNCT "(")
(ID "x")
(PUNCT ",")
(ID "str")
(PUNCT ")")
(KEYWORD and)
(ID "x")
(KEYWORD in)
(ID "common_names")
(PUNCT ")")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "subpatterns_with_common_names")
(PUNCT ".")
(ID "append")
(PUNCT "(")
(ID "subpattern")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "subpatterns_with_names")
(PUNCT ".")
(ID "append")
(PUNCT "(")
(ID "subpattern")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(DEDENT)
(KEYWORD if)
(ID "subpatterns_with_names")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "subpatterns")
(PUNCT "=")
(ID "subpatterns_with_names")
(NEWLINE)
(DEDENT)
(KEYWORD elif)
(ID "subpatterns_with_common_names")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "subpatterns")
(PUNCT "=")
(ID "subpatterns_with_common_names")
(NEWLINE)
(DEDENT)
(KEYWORD elif)
(ID "subpatterns_with_common_chars")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "subpatterns")
(PUNCT "=")
(ID "subpatterns_with_common_chars")
(NEWLINE)
(DEDENT)
(KEYWORD return)
(ID "max")
(PUNCT "(")
(ID "subpatterns")
(PUNCT ",")
(ID "key")
(PUNCT "=")
(ID "len")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "rec_test")
(PUNCT "(")
(ID "sequence")
(PUNCT ",")
(ID "test_func")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Tests test_func on all items of sequence and items of included\n    sub-iterables")
(NEWLINE)
(KEYWORD for)
(ID "x")
(KEYWORD in)
(ID "sequence")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "isinstance")
(PUNCT "(")
(ID "x")
(PUNCT ",")
(PUNCT "(")
(ID "list")
(PUNCT ",")
(ID "tuple")
(PUNCT ")")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD yield)
(KEYWORD from)
(ID "rec_test")
(PUNCT "(")
(ID "x")
(PUNCT ",")
(ID "test_func")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD yield)
(ID "test_func")
(PUNCT "(")
(ID "x")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(DEDENT)
(ENDMARKER)
