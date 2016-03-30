(LIT "A bottom-up tree matching algorithm implementation meant to speed\nup 2to3's matching process. After the tree patterns are reduced to\ntheir rarest linear path, a linear Aho-Corasick automaton is\ncreated. The linear automaton traverses the linear paths from the\nleaves to the root of the AST and returns a set of nodes for further\nmatching. This reduces significantly the number of candidate nodes.")
(NEWLINE)
(ID "__author__")
(PUNCT "=")
(LIT "George Boutsioukis <gboutsioukis@gmail.com>")
(NEWLINE)
(KEYWORD import)
(ID "logging")
(NEWLINE)
(KEYWORD import)
(ID "itertools")
(NEWLINE)
(KEYWORD from)
(ID "collections")
(KEYWORD import)
(ID "defaultdict")
(NEWLINE)
(KEYWORD from)
(PUNCT ".")
(KEYWORD import)
(ID "pytree")
(NEWLINE)
(KEYWORD from)
(PUNCT ".")
(ID "btm_utils")
(KEYWORD import)
(ID "reduce_tree")
(NEWLINE)
(KEYWORD class)
(ID "BMNode")
(PUNCT "(")
(ID "object")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Class for a node of the Aho-Corasick automaton used in matching")
(NEWLINE)
(ID "count")
(PUNCT "=")
(ID "itertools")
(PUNCT ".")
(ID "count")
(PUNCT "(")
(PUNCT ")")
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
(ID "transition_table")
(PUNCT "=")
(PUNCT "{")
(PUNCT "}")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "fixers")
(PUNCT "=")
(PUNCT "[")
(PUNCT "]")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "id")
(PUNCT "=")
(ID "next")
(PUNCT "(")
(ID "BMNode")
(PUNCT ".")
(ID "count")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "content")
(PUNCT "=")
(LIT "")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD class)
(ID "BottomMatcher")
(PUNCT "(")
(ID "object")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "The main matcher class. After instantiating the patterns should\n    be added using the add_fixer method")
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
(ID "match")
(PUNCT "=")
(ID "set")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "root")
(PUNCT "=")
(ID "BMNode")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "nodes")
(PUNCT "=")
(PUNCT "[")
(ID "self")
(PUNCT ".")
(ID "root")
(PUNCT "]")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "fixers")
(PUNCT "=")
(PUNCT "[")
(PUNCT "]")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "logger")
(PUNCT "=")
(ID "logging")
(PUNCT ".")
(ID "getLogger")
(PUNCT "(")
(LIT "RefactoringTool")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "add_fixer")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "fixer")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Reduces a fixer's pattern tree to a linear path and adds it\n        to the matcher(a common Aho-Corasick automaton). The fixer is\n        appended on the matching states and called when they are\n        reached")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "fixers")
(PUNCT ".")
(ID "append")
(PUNCT "(")
(ID "fixer")
(PUNCT ")")
(NEWLINE)
(ID "tree")
(PUNCT "=")
(ID "reduce_tree")
(PUNCT "(")
(ID "fixer")
(PUNCT ".")
(ID "pattern_tree")
(PUNCT ")")
(NEWLINE)
(ID "linear")
(PUNCT "=")
(ID "tree")
(PUNCT ".")
(ID "get_linear_subpattern")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "match_nodes")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "add")
(PUNCT "(")
(ID "linear")
(PUNCT ",")
(ID "start")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "root")
(PUNCT ")")
(NEWLINE)
(KEYWORD for)
(ID "match_node")
(KEYWORD in)
(ID "match_nodes")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "match_node")
(PUNCT ".")
(ID "fixers")
(PUNCT ".")
(ID "append")
(PUNCT "(")
(ID "fixer")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD def)
(ID "add")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "pattern")
(PUNCT ",")
(ID "start")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Recursively adds a linear pattern to the AC automaton")
(NEWLINE)
(KEYWORD if)
(KEYWORD not)
(ID "pattern")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(PUNCT "[")
(ID "start")
(PUNCT "]")
(NEWLINE)
(DEDENT)
(KEYWORD if)
(ID "isinstance")
(PUNCT "(")
(ID "pattern")
(PUNCT "[")
(LIT 0)
(PUNCT "]")
(PUNCT ",")
(ID "tuple")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "match_nodes")
(PUNCT "=")
(PUNCT "[")
(PUNCT "]")
(NEWLINE)
(KEYWORD for)
(ID "alternative")
(KEYWORD in)
(ID "pattern")
(PUNCT "[")
(LIT 0)
(PUNCT "]")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "end_nodes")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "add")
(PUNCT "(")
(ID "alternative")
(PUNCT ",")
(ID "start")
(PUNCT "=")
(ID "start")
(PUNCT ")")
(NEWLINE)
(KEYWORD for)
(ID "end")
(KEYWORD in)
(ID "end_nodes")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "match_nodes")
(PUNCT ".")
(ID "extend")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "add")
(PUNCT "(")
(ID "pattern")
(PUNCT "[")
(LIT 1)
(PUNCT ":")
(PUNCT "]")
(PUNCT ",")
(ID "end")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD return)
(ID "match_nodes")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "pattern")
(PUNCT "[")
(LIT 0)
(PUNCT "]")
(KEYWORD not)
(KEYWORD in)
(ID "start")
(PUNCT ".")
(ID "transition_table")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "next_node")
(PUNCT "=")
(ID "BMNode")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "start")
(PUNCT ".")
(ID "transition_table")
(PUNCT "[")
(ID "pattern")
(PUNCT "[")
(LIT 0)
(PUNCT "]")
(PUNCT "]")
(PUNCT "=")
(ID "next_node")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "next_node")
(PUNCT "=")
(ID "start")
(PUNCT ".")
(ID "transition_table")
(PUNCT "[")
(ID "pattern")
(PUNCT "[")
(LIT 0)
(PUNCT "]")
(PUNCT "]")
(NEWLINE)
(DEDENT)
(KEYWORD if)
(ID "pattern")
(PUNCT "[")
(LIT 1)
(PUNCT ":")
(PUNCT "]")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "end_nodes")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "add")
(PUNCT "(")
(ID "pattern")
(PUNCT "[")
(LIT 1)
(PUNCT ":")
(PUNCT "]")
(PUNCT ",")
(ID "start")
(PUNCT "=")
(ID "next_node")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "end_nodes")
(PUNCT "=")
(PUNCT "[")
(ID "next_node")
(PUNCT "]")
(NEWLINE)
(DEDENT)
(KEYWORD return)
(ID "end_nodes")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD def)
(ID "run")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "leaves")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "The main interface with the bottom matcher. The tree is\n        traversed from the bottom using the constructed\n        automaton. Nodes are only checked once as the tree is\n        retraversed. When the automaton fails, we give it one more\n        shot(in case the above tree matches as a whole with the\n        rejected leaf), then we break for the next leaf. There is the\n        special case of multiple arguments(see code comments) where we\n        recheck the nodes\n\n        Args:\n           The leaves of the AST tree to be matched\n\n        Returns:\n           A dictionary of node matches with fixers as the keys\n        ")
(NEWLINE)
(ID "current_ac_node")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "root")
(NEWLINE)
(ID "results")
(PUNCT "=")
(ID "defaultdict")
(PUNCT "(")
(ID "list")
(PUNCT ")")
(NEWLINE)
(KEYWORD for)
(ID "leaf")
(KEYWORD in)
(ID "leaves")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "current_ast_node")
(PUNCT "=")
(ID "leaf")
(NEWLINE)
(KEYWORD while)
(ID "current_ast_node")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "current_ast_node")
(PUNCT ".")
(ID "was_checked")
(PUNCT "=")
(KEYWORD True)
(NEWLINE)
(KEYWORD for)
(ID "child")
(KEYWORD in)
(ID "current_ast_node")
(PUNCT ".")
(ID "children")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "isinstance")
(PUNCT "(")
(ID "child")
(PUNCT ",")
(ID "pytree")
(PUNCT ".")
(ID "Leaf")
(PUNCT ")")
(KEYWORD and)
(ID "child")
(PUNCT ".")
(ID "value")
(PUNCT "==")
(LIT ";")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "current_ast_node")
(PUNCT ".")
(ID "was_checked")
(PUNCT "=")
(KEYWORD False)
(NEWLINE)
(KEYWORD break)
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD if)
(ID "current_ast_node")
(PUNCT ".")
(ID "type")
(PUNCT "==")
(LIT 1)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "node_token")
(PUNCT "=")
(ID "current_ast_node")
(PUNCT ".")
(ID "value")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "node_token")
(PUNCT "=")
(ID "current_ast_node")
(PUNCT ".")
(ID "type")
(NEWLINE)
(DEDENT)
(KEYWORD if)
(ID "node_token")
(KEYWORD in)
(ID "current_ac_node")
(PUNCT ".")
(ID "transition_table")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "current_ac_node")
(PUNCT "=")
(ID "current_ac_node")
(PUNCT ".")
(ID "transition_table")
(PUNCT "[")
(ID "node_token")
(PUNCT "]")
(NEWLINE)
(KEYWORD for)
(ID "fixer")
(KEYWORD in)
(ID "current_ac_node")
(PUNCT ".")
(ID "fixers")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(KEYWORD not)
(ID "fixer")
(KEYWORD in)
(ID "results")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "results")
(PUNCT "[")
(ID "fixer")
(PUNCT "]")
(PUNCT "=")
(PUNCT "[")
(PUNCT "]")
(NEWLINE)
(DEDENT)
(ID "results")
(PUNCT "[")
(ID "fixer")
(PUNCT "]")
(PUNCT ".")
(ID "append")
(PUNCT "(")
(ID "current_ast_node")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "current_ac_node")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "root")
(NEWLINE)
(KEYWORD if)
(PUNCT "(")
(ID "current_ast_node")
(PUNCT ".")
(ID "parent")
(KEYWORD is)
(KEYWORD not)
(KEYWORD None)
(KEYWORD and)
(ID "current_ast_node")
(PUNCT ".")
(ID "parent")
(PUNCT ".")
(ID "was_checked")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD break)
(NEWLINE)
(DEDENT)
(KEYWORD if)
(ID "node_token")
(KEYWORD in)
(ID "current_ac_node")
(PUNCT ".")
(ID "transition_table")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "current_ac_node")
(PUNCT "=")
(ID "current_ac_node")
(PUNCT ".")
(ID "transition_table")
(PUNCT "[")
(ID "node_token")
(PUNCT "]")
(NEWLINE)
(KEYWORD for)
(ID "fixer")
(KEYWORD in)
(ID "current_ac_node")
(PUNCT ".")
(ID "fixers")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(KEYWORD not)
(ID "fixer")
(KEYWORD in)
(ID "results")
(PUNCT ".")
(ID "keys")
(PUNCT "(")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "results")
(PUNCT "[")
(ID "fixer")
(PUNCT "]")
(PUNCT "=")
(PUNCT "[")
(PUNCT "]")
(NEWLINE)
(DEDENT)
(ID "results")
(PUNCT "[")
(ID "fixer")
(PUNCT "]")
(PUNCT ".")
(ID "append")
(PUNCT "(")
(ID "current_ast_node")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(DEDENT)
(ID "current_ast_node")
(PUNCT "=")
(ID "current_ast_node")
(PUNCT ".")
(ID "parent")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD return)
(ID "results")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "print_ac")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Prints a graphviz diagram of the BM automaton(for debugging)")
(NEWLINE)
(ID "print")
(PUNCT "(")
(LIT "digraph g{")
(PUNCT ")")
(NEWLINE)
(KEYWORD def)
(ID "print_node")
(PUNCT "(")
(ID "node")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD for)
(ID "subnode_key")
(KEYWORD in)
(ID "node")
(PUNCT ".")
(ID "transition_table")
(PUNCT ".")
(ID "keys")
(PUNCT "(")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "subnode")
(PUNCT "=")
(ID "node")
(PUNCT ".")
(ID "transition_table")
(PUNCT "[")
(ID "subnode_key")
(PUNCT "]")
(NEWLINE)
(ID "print")
(PUNCT "(")
(LIT "%d -> %d [label=%s] //%s")
(PUNCT "%")
(PUNCT "(")
(ID "node")
(PUNCT ".")
(ID "id")
(PUNCT ",")
(ID "subnode")
(PUNCT ".")
(ID "id")
(PUNCT ",")
(ID "type_repr")
(PUNCT "(")
(ID "subnode_key")
(PUNCT ")")
(PUNCT ",")
(ID "str")
(PUNCT "(")
(ID "subnode")
(PUNCT ".")
(ID "fixers")
(PUNCT ")")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(ID "subnode_key")
(PUNCT "==")
(LIT 1)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "print")
(PUNCT "(")
(ID "subnode")
(PUNCT ".")
(ID "content")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "print_node")
(PUNCT "(")
(ID "subnode")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(ID "print_node")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "root")
(PUNCT ")")
(NEWLINE)
(ID "print")
(PUNCT "(")
(LIT "}")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(ID "_type_reprs")
(PUNCT "=")
(PUNCT "{")
(PUNCT "}")
(NEWLINE)
(KEYWORD def)
(ID "type_repr")
(PUNCT "(")
(ID "type_num")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD global)
(ID "_type_reprs")
(NEWLINE)
(KEYWORD if)
(KEYWORD not)
(ID "_type_reprs")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD from)
(PUNCT ".")
(ID "pygram")
(KEYWORD import)
(ID "python_symbols")
(NEWLINE)
(KEYWORD for)
(ID "name")
(PUNCT ",")
(ID "val")
(KEYWORD in)
(ID "python_symbols")
(PUNCT ".")
(ID "__dict__")
(PUNCT ".")
(ID "items")
(PUNCT "(")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "type")
(PUNCT "(")
(ID "val")
(PUNCT ")")
(PUNCT "==")
(ID "int")
(PUNCT ":")
(ID "_type_reprs")
(PUNCT "[")
(ID "val")
(PUNCT "]")
(PUNCT "=")
(ID "name")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD return)
(ID "_type_reprs")
(PUNCT ".")
(ID "setdefault")
(PUNCT "(")
(ID "type_num")
(PUNCT ",")
(ID "type_num")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ENDMARKER)
