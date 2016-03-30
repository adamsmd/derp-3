(LIT "Bisection algorithms.")
(NEWLINE)
(KEYWORD def)
(ID "insort_right")
(PUNCT "(")
(ID "a")
(PUNCT ",")
(ID "x")
(PUNCT ",")
(ID "lo")
(PUNCT "=")
(LIT 0)
(PUNCT ",")
(ID "hi")
(PUNCT "=")
(KEYWORD None)
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Insert item x in list a, and keep it sorted assuming a is sorted.\n\n    If x is already in a, insert it to the right of the rightmost x.\n\n    Optional args lo (default 0) and hi (default len(a)) bound the\n    slice of a to be searched.\n    ")
(NEWLINE)
(KEYWORD if)
(ID "lo")
(PUNCT "<")
(LIT 0)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD raise)
(ID "ValueError")
(PUNCT "(")
(LIT "lo must be non-negative")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD if)
(ID "hi")
(KEYWORD is)
(KEYWORD None)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "hi")
(PUNCT "=")
(ID "len")
(PUNCT "(")
(ID "a")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD while)
(ID "lo")
(PUNCT "<")
(ID "hi")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "mid")
(PUNCT "=")
(PUNCT "(")
(ID "lo")
(PUNCT "+")
(ID "hi")
(PUNCT ")")
(PUNCT "//")
(LIT 2)
(NEWLINE)
(KEYWORD if)
(ID "x")
(PUNCT "<")
(ID "a")
(PUNCT "[")
(ID "mid")
(PUNCT "]")
(PUNCT ":")
(ID "hi")
(PUNCT "=")
(ID "mid")
(NEWLINE)
(KEYWORD else)
(PUNCT ":")
(ID "lo")
(PUNCT "=")
(ID "mid")
(PUNCT "+")
(LIT 1)
(NEWLINE)
(DEDENT)
(ID "a")
(PUNCT ".")
(ID "insert")
(PUNCT "(")
(ID "lo")
(PUNCT ",")
(ID "x")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "insort")
(PUNCT "=")
(ID "insort_right")
(NEWLINE)
(KEYWORD def)
(ID "bisect_right")
(PUNCT "(")
(ID "a")
(PUNCT ",")
(ID "x")
(PUNCT ",")
(ID "lo")
(PUNCT "=")
(LIT 0)
(PUNCT ",")
(ID "hi")
(PUNCT "=")
(KEYWORD None)
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Return the index where to insert item x in list a, assuming a is sorted.\n\n    The return value i is such that all e in a[:i] have e <= x, and all e in\n    a[i:] have e > x.  So if x already appears in the list, a.insert(x) will\n    insert just after the rightmost x already there.\n\n    Optional args lo (default 0) and hi (default len(a)) bound the\n    slice of a to be searched.\n    ")
(NEWLINE)
(KEYWORD if)
(ID "lo")
(PUNCT "<")
(LIT 0)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD raise)
(ID "ValueError")
(PUNCT "(")
(LIT "lo must be non-negative")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD if)
(ID "hi")
(KEYWORD is)
(KEYWORD None)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "hi")
(PUNCT "=")
(ID "len")
(PUNCT "(")
(ID "a")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD while)
(ID "lo")
(PUNCT "<")
(ID "hi")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "mid")
(PUNCT "=")
(PUNCT "(")
(ID "lo")
(PUNCT "+")
(ID "hi")
(PUNCT ")")
(PUNCT "//")
(LIT 2)
(NEWLINE)
(KEYWORD if)
(ID "x")
(PUNCT "<")
(ID "a")
(PUNCT "[")
(ID "mid")
(PUNCT "]")
(PUNCT ":")
(ID "hi")
(PUNCT "=")
(ID "mid")
(NEWLINE)
(KEYWORD else)
(PUNCT ":")
(ID "lo")
(PUNCT "=")
(ID "mid")
(PUNCT "+")
(LIT 1)
(NEWLINE)
(DEDENT)
(KEYWORD return)
(ID "lo")
(NEWLINE)
(DEDENT)
(ID "bisect")
(PUNCT "=")
(ID "bisect_right")
(NEWLINE)
(KEYWORD def)
(ID "insort_left")
(PUNCT "(")
(ID "a")
(PUNCT ",")
(ID "x")
(PUNCT ",")
(ID "lo")
(PUNCT "=")
(LIT 0)
(PUNCT ",")
(ID "hi")
(PUNCT "=")
(KEYWORD None)
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Insert item x in list a, and keep it sorted assuming a is sorted.\n\n    If x is already in a, insert it to the left of the leftmost x.\n\n    Optional args lo (default 0) and hi (default len(a)) bound the\n    slice of a to be searched.\n    ")
(NEWLINE)
(KEYWORD if)
(ID "lo")
(PUNCT "<")
(LIT 0)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD raise)
(ID "ValueError")
(PUNCT "(")
(LIT "lo must be non-negative")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD if)
(ID "hi")
(KEYWORD is)
(KEYWORD None)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "hi")
(PUNCT "=")
(ID "len")
(PUNCT "(")
(ID "a")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD while)
(ID "lo")
(PUNCT "<")
(ID "hi")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "mid")
(PUNCT "=")
(PUNCT "(")
(ID "lo")
(PUNCT "+")
(ID "hi")
(PUNCT ")")
(PUNCT "//")
(LIT 2)
(NEWLINE)
(KEYWORD if)
(ID "a")
(PUNCT "[")
(ID "mid")
(PUNCT "]")
(PUNCT "<")
(ID "x")
(PUNCT ":")
(ID "lo")
(PUNCT "=")
(ID "mid")
(PUNCT "+")
(LIT 1)
(NEWLINE)
(KEYWORD else)
(PUNCT ":")
(ID "hi")
(PUNCT "=")
(ID "mid")
(NEWLINE)
(DEDENT)
(ID "a")
(PUNCT ".")
(ID "insert")
(PUNCT "(")
(ID "lo")
(PUNCT ",")
(ID "x")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "bisect_left")
(PUNCT "(")
(ID "a")
(PUNCT ",")
(ID "x")
(PUNCT ",")
(ID "lo")
(PUNCT "=")
(LIT 0)
(PUNCT ",")
(ID "hi")
(PUNCT "=")
(KEYWORD None)
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Return the index where to insert item x in list a, assuming a is sorted.\n\n    The return value i is such that all e in a[:i] have e < x, and all e in\n    a[i:] have e >= x.  So if x already appears in the list, a.insert(x) will\n    insert just before the leftmost x already there.\n\n    Optional args lo (default 0) and hi (default len(a)) bound the\n    slice of a to be searched.\n    ")
(NEWLINE)
(KEYWORD if)
(ID "lo")
(PUNCT "<")
(LIT 0)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD raise)
(ID "ValueError")
(PUNCT "(")
(LIT "lo must be non-negative")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD if)
(ID "hi")
(KEYWORD is)
(KEYWORD None)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "hi")
(PUNCT "=")
(ID "len")
(PUNCT "(")
(ID "a")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD while)
(ID "lo")
(PUNCT "<")
(ID "hi")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "mid")
(PUNCT "=")
(PUNCT "(")
(ID "lo")
(PUNCT "+")
(ID "hi")
(PUNCT ")")
(PUNCT "//")
(LIT 2)
(NEWLINE)
(KEYWORD if)
(ID "a")
(PUNCT "[")
(ID "mid")
(PUNCT "]")
(PUNCT "<")
(ID "x")
(PUNCT ":")
(ID "lo")
(PUNCT "=")
(ID "mid")
(PUNCT "+")
(LIT 1)
(NEWLINE)
(KEYWORD else)
(PUNCT ":")
(ID "hi")
(PUNCT "=")
(ID "mid")
(NEWLINE)
(DEDENT)
(KEYWORD return)
(ID "lo")
(NEWLINE)
(DEDENT)
(KEYWORD try)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD from)
(ID "_bisect")
(KEYWORD import)
(PUNCT "*")
(NEWLINE)
(DEDENT)
(KEYWORD except)
(ID "ImportError")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD pass)
(NEWLINE)
(DEDENT)
(ENDMARKER)
