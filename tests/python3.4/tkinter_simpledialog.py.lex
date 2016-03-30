(LIT "This modules handles dialog boxes.\n\nIt contains the following public symbols:\n\nSimpleDialog -- A simple but flexible modal dialog box\n\nDialog -- a base class for dialogs\n\naskinteger -- get an integer from the user\n\naskfloat -- get a float from the user\n\naskstring -- get a string from the user\n")
(NEWLINE)
(KEYWORD from)
(ID "tkinter")
(KEYWORD import)
(PUNCT "*")
(NEWLINE)
(KEYWORD from)
(ID "tkinter")
(KEYWORD import)
(ID "messagebox")
(NEWLINE)
(KEYWORD import)
(ID "tkinter")
(NEWLINE)
(KEYWORD class)
(ID "SimpleDialog")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD def)
(ID "__init__")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "master")
(PUNCT ",")
(ID "text")
(PUNCT "=")
(LIT "")
(PUNCT ",")
(ID "buttons")
(PUNCT "=")
(PUNCT "[")
(PUNCT "]")
(PUNCT ",")
(ID "default")
(PUNCT "=")
(KEYWORD None)
(PUNCT ",")
(ID "cancel")
(PUNCT "=")
(KEYWORD None)
(PUNCT ",")
(ID "title")
(PUNCT "=")
(KEYWORD None)
(PUNCT ",")
(ID "class_")
(PUNCT "=")
(KEYWORD None)
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "class_")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "root")
(PUNCT "=")
(ID "Toplevel")
(PUNCT "(")
(ID "master")
(PUNCT ",")
(ID "class_")
(PUNCT "=")
(ID "class_")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "root")
(PUNCT "=")
(ID "Toplevel")
(PUNCT "(")
(ID "master")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD if)
(ID "title")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "root")
(PUNCT ".")
(ID "title")
(PUNCT "(")
(ID "title")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "root")
(PUNCT ".")
(ID "iconname")
(PUNCT "(")
(ID "title")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "self")
(PUNCT ".")
(ID "message")
(PUNCT "=")
(ID "Message")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "root")
(PUNCT ",")
(ID "text")
(PUNCT "=")
(ID "text")
(PUNCT ",")
(ID "aspect")
(PUNCT "=")
(LIT 400)
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "message")
(PUNCT ".")
(ID "pack")
(PUNCT "(")
(ID "expand")
(PUNCT "=")
(LIT 1)
(PUNCT ",")
(ID "fill")
(PUNCT "=")
(ID "BOTH")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "frame")
(PUNCT "=")
(ID "Frame")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "root")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "frame")
(PUNCT ".")
(ID "pack")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "num")
(PUNCT "=")
(ID "default")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "cancel")
(PUNCT "=")
(ID "cancel")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "default")
(PUNCT "=")
(ID "default")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "root")
(PUNCT ".")
(ID "bind")
(PUNCT "(")
(LIT "<Return>")
(PUNCT ",")
(ID "self")
(PUNCT ".")
(ID "return_event")
(PUNCT ")")
(NEWLINE)
(KEYWORD for)
(ID "num")
(KEYWORD in)
(ID "range")
(PUNCT "(")
(ID "len")
(PUNCT "(")
(ID "buttons")
(PUNCT ")")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "s")
(PUNCT "=")
(ID "buttons")
(PUNCT "[")
(ID "num")
(PUNCT "]")
(NEWLINE)
(ID "b")
(PUNCT "=")
(ID "Button")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "frame")
(PUNCT ",")
(ID "text")
(PUNCT "=")
(ID "s")
(PUNCT ",")
(ID "command")
(PUNCT "=")
(PUNCT "(")
(KEYWORD lambda)
(ID "self")
(PUNCT "=")
(ID "self")
(PUNCT ",")
(ID "num")
(PUNCT "=")
(ID "num")
(PUNCT ":")
(ID "self")
(PUNCT ".")
(ID "done")
(PUNCT "(")
(ID "num")
(PUNCT ")")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(ID "num")
(PUNCT "==")
(ID "default")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "b")
(PUNCT ".")
(ID "config")
(PUNCT "(")
(ID "relief")
(PUNCT "=")
(ID "RIDGE")
(PUNCT ",")
(ID "borderwidth")
(PUNCT "=")
(LIT 8)
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "b")
(PUNCT ".")
(ID "pack")
(PUNCT "(")
(ID "side")
(PUNCT "=")
(ID "LEFT")
(PUNCT ",")
(ID "fill")
(PUNCT "=")
(ID "BOTH")
(PUNCT ",")
(ID "expand")
(PUNCT "=")
(LIT 1)
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "self")
(PUNCT ".")
(ID "root")
(PUNCT ".")
(ID "protocol")
(PUNCT "(")
(LIT "WM_DELETE_WINDOW")
(PUNCT ",")
(ID "self")
(PUNCT ".")
(ID "wm_delete_window")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_set_transient")
(PUNCT "(")
(ID "master")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "_set_transient")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "master")
(PUNCT ",")
(ID "relx")
(PUNCT "=")
(LIT 0.5)
(PUNCT ",")
(ID "rely")
(PUNCT "=")
(LIT 0.3)
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "widget")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "root")
(NEWLINE)
(ID "widget")
(PUNCT ".")
(ID "withdraw")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "widget")
(PUNCT ".")
(ID "transient")
(PUNCT "(")
(ID "master")
(PUNCT ")")
(NEWLINE)
(ID "widget")
(PUNCT ".")
(ID "update_idletasks")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(ID "master")
(PUNCT ".")
(ID "winfo_ismapped")
(PUNCT "(")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "m_width")
(PUNCT "=")
(ID "master")
(PUNCT ".")
(ID "winfo_width")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "m_height")
(PUNCT "=")
(ID "master")
(PUNCT ".")
(ID "winfo_height")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "m_x")
(PUNCT "=")
(ID "master")
(PUNCT ".")
(ID "winfo_rootx")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "m_y")
(PUNCT "=")
(ID "master")
(PUNCT ".")
(ID "winfo_rooty")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "m_width")
(PUNCT "=")
(ID "master")
(PUNCT ".")
(ID "winfo_screenwidth")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "m_height")
(PUNCT "=")
(ID "master")
(PUNCT ".")
(ID "winfo_screenheight")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "m_x")
(PUNCT "=")
(ID "m_y")
(PUNCT "=")
(LIT 0)
(NEWLINE)
(DEDENT)
(ID "w_width")
(PUNCT "=")
(ID "widget")
(PUNCT ".")
(ID "winfo_reqwidth")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "w_height")
(PUNCT "=")
(ID "widget")
(PUNCT ".")
(ID "winfo_reqheight")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "x")
(PUNCT "=")
(ID "m_x")
(PUNCT "+")
(PUNCT "(")
(ID "m_width")
(PUNCT "-")
(ID "w_width")
(PUNCT ")")
(PUNCT "*")
(ID "relx")
(NEWLINE)
(ID "y")
(PUNCT "=")
(ID "m_y")
(PUNCT "+")
(PUNCT "(")
(ID "m_height")
(PUNCT "-")
(ID "w_height")
(PUNCT ")")
(PUNCT "*")
(ID "rely")
(NEWLINE)
(KEYWORD if)
(ID "x")
(PUNCT "+")
(ID "w_width")
(PUNCT ">")
(ID "master")
(PUNCT ".")
(ID "winfo_screenwidth")
(PUNCT "(")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "x")
(PUNCT "=")
(ID "master")
(PUNCT ".")
(ID "winfo_screenwidth")
(PUNCT "(")
(PUNCT ")")
(PUNCT "-")
(ID "w_width")
(NEWLINE)
(DEDENT)
(KEYWORD elif)
(ID "x")
(PUNCT "<")
(LIT 0)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "x")
(PUNCT "=")
(LIT 0)
(NEWLINE)
(DEDENT)
(KEYWORD if)
(ID "y")
(PUNCT "+")
(ID "w_height")
(PUNCT ">")
(ID "master")
(PUNCT ".")
(ID "winfo_screenheight")
(PUNCT "(")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "y")
(PUNCT "=")
(ID "master")
(PUNCT ".")
(ID "winfo_screenheight")
(PUNCT "(")
(PUNCT ")")
(PUNCT "-")
(ID "w_height")
(NEWLINE)
(DEDENT)
(KEYWORD elif)
(ID "y")
(PUNCT "<")
(LIT 0)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "y")
(PUNCT "=")
(LIT 0)
(NEWLINE)
(DEDENT)
(ID "widget")
(PUNCT ".")
(ID "geometry")
(PUNCT "(")
(LIT "+%d+%d")
(PUNCT "%")
(PUNCT "(")
(ID "x")
(PUNCT ",")
(ID "y")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(ID "widget")
(PUNCT ".")
(ID "deiconify")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "go")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "root")
(PUNCT ".")
(ID "wait_visibility")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "root")
(PUNCT ".")
(ID "grab_set")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "root")
(PUNCT ".")
(ID "mainloop")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "root")
(PUNCT ".")
(ID "destroy")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(KEYWORD return)
(ID "self")
(PUNCT ".")
(ID "num")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "return_event")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "event")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "self")
(PUNCT ".")
(ID "default")
(KEYWORD is)
(KEYWORD None)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "root")
(PUNCT ".")
(ID "bell")
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
(ID "done")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "default")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD def)
(ID "wm_delete_window")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "self")
(PUNCT ".")
(ID "cancel")
(KEYWORD is)
(KEYWORD None)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "root")
(PUNCT ".")
(ID "bell")
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
(ID "done")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "cancel")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD def)
(ID "done")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "num")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "num")
(PUNCT "=")
(ID "num")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "root")
(PUNCT ".")
(ID "quit")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD class)
(ID "Dialog")
(PUNCT "(")
(ID "Toplevel")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Class to open dialogs.\n\n    This class is intended as a base class for custom dialogs\n    ")
(NEWLINE)
(KEYWORD def)
(ID "__init__")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "parent")
(PUNCT ",")
(ID "title")
(PUNCT "=")
(KEYWORD None)
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Initialize a dialog.\n\n        Arguments:\n\n            parent -- a parent window (the application window)\n\n            title -- the dialog title\n        ")
(NEWLINE)
(ID "Toplevel")
(PUNCT ".")
(ID "__init__")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "parent")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "withdraw")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(ID "parent")
(PUNCT ".")
(ID "winfo_viewable")
(PUNCT "(")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "transient")
(PUNCT "(")
(ID "parent")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD if)
(ID "title")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "title")
(PUNCT "(")
(ID "title")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "self")
(PUNCT ".")
(ID "parent")
(PUNCT "=")
(ID "parent")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "result")
(PUNCT "=")
(KEYWORD None)
(NEWLINE)
(ID "body")
(PUNCT "=")
(ID "Frame")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "initial_focus")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "body")
(PUNCT "(")
(ID "body")
(PUNCT ")")
(NEWLINE)
(ID "body")
(PUNCT ".")
(ID "pack")
(PUNCT "(")
(ID "padx")
(PUNCT "=")
(LIT 5)
(PUNCT ",")
(ID "pady")
(PUNCT "=")
(LIT 5)
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "buttonbox")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(KEYWORD not)
(ID "self")
(PUNCT ".")
(ID "initial_focus")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "initial_focus")
(PUNCT "=")
(ID "self")
(NEWLINE)
(DEDENT)
(ID "self")
(PUNCT ".")
(ID "protocol")
(PUNCT "(")
(LIT "WM_DELETE_WINDOW")
(PUNCT ",")
(ID "self")
(PUNCT ".")
(ID "cancel")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(ID "self")
(PUNCT ".")
(ID "parent")
(KEYWORD is)
(KEYWORD not)
(KEYWORD None)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "geometry")
(PUNCT "(")
(LIT "+%d+%d")
(PUNCT "%")
(PUNCT "(")
(ID "parent")
(PUNCT ".")
(ID "winfo_rootx")
(PUNCT "(")
(PUNCT ")")
(PUNCT "+")
(LIT 50)
(PUNCT ",")
(ID "parent")
(PUNCT ".")
(ID "winfo_rooty")
(PUNCT "(")
(PUNCT ")")
(PUNCT "+")
(LIT 50)
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "self")
(PUNCT ".")
(ID "deiconify")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "initial_focus")
(PUNCT ".")
(ID "focus_set")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "wait_visibility")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "grab_set")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "wait_window")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "destroy")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Destroy the window")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "initial_focus")
(PUNCT "=")
(KEYWORD None)
(NEWLINE)
(ID "Toplevel")
(PUNCT ".")
(ID "destroy")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "body")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "master")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "create dialog body.\n\n        return widget that should have initial focus.\n        This method should be overridden, and is called\n        by the __init__ method.\n        ")
(NEWLINE)
(KEYWORD pass)
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "buttonbox")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "add standard button box.\n\n        override if you do not want the standard buttons\n        ")
(NEWLINE)
(ID "box")
(PUNCT "=")
(ID "Frame")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(NEWLINE)
(ID "w")
(PUNCT "=")
(ID "Button")
(PUNCT "(")
(ID "box")
(PUNCT ",")
(ID "text")
(PUNCT "=")
(LIT "OK")
(PUNCT ",")
(ID "width")
(PUNCT "=")
(LIT 10)
(PUNCT ",")
(ID "command")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "ok")
(PUNCT ",")
(ID "default")
(PUNCT "=")
(ID "ACTIVE")
(PUNCT ")")
(NEWLINE)
(ID "w")
(PUNCT ".")
(ID "pack")
(PUNCT "(")
(ID "side")
(PUNCT "=")
(ID "LEFT")
(PUNCT ",")
(ID "padx")
(PUNCT "=")
(LIT 5)
(PUNCT ",")
(ID "pady")
(PUNCT "=")
(LIT 5)
(PUNCT ")")
(NEWLINE)
(ID "w")
(PUNCT "=")
(ID "Button")
(PUNCT "(")
(ID "box")
(PUNCT ",")
(ID "text")
(PUNCT "=")
(LIT "Cancel")
(PUNCT ",")
(ID "width")
(PUNCT "=")
(LIT 10)
(PUNCT ",")
(ID "command")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "cancel")
(PUNCT ")")
(NEWLINE)
(ID "w")
(PUNCT ".")
(ID "pack")
(PUNCT "(")
(ID "side")
(PUNCT "=")
(ID "LEFT")
(PUNCT ",")
(ID "padx")
(PUNCT "=")
(LIT 5)
(PUNCT ",")
(ID "pady")
(PUNCT "=")
(LIT 5)
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "bind")
(PUNCT "(")
(LIT "<Return>")
(PUNCT ",")
(ID "self")
(PUNCT ".")
(ID "ok")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "bind")
(PUNCT "(")
(LIT "<Escape>")
(PUNCT ",")
(ID "self")
(PUNCT ".")
(ID "cancel")
(PUNCT ")")
(NEWLINE)
(ID "box")
(PUNCT ".")
(ID "pack")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "ok")
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
(KEYWORD not)
(ID "self")
(PUNCT ".")
(ID "validate")
(PUNCT "(")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "initial_focus")
(PUNCT ".")
(ID "focus_set")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(KEYWORD return)
(NEWLINE)
(DEDENT)
(ID "self")
(PUNCT ".")
(ID "withdraw")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "update_idletasks")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(KEYWORD try)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "apply")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD finally)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "cancel")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD def)
(ID "cancel")
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
(ID "parent")
(KEYWORD is)
(KEYWORD not)
(KEYWORD None)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "parent")
(PUNCT ".")
(ID "focus_set")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "self")
(PUNCT ".")
(ID "destroy")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "validate")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "validate the data\n\n        This method is called automatically to validate the data before the\n        dialog is destroyed. By default, it always validates OK.\n        ")
(NEWLINE)
(KEYWORD return)
(LIT 1)
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "apply")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "process the data\n\n        This method is called automatically to process the data, *after*\n        the dialog is destroyed. By default, it does nothing.\n        ")
(NEWLINE)
(KEYWORD pass)
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD class)
(ID "_QueryDialog")
(PUNCT "(")
(ID "Dialog")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD def)
(ID "__init__")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "title")
(PUNCT ",")
(ID "prompt")
(PUNCT ",")
(ID "initialvalue")
(PUNCT "=")
(KEYWORD None)
(PUNCT ",")
(ID "minvalue")
(PUNCT "=")
(KEYWORD None)
(PUNCT ",")
(ID "maxvalue")
(PUNCT "=")
(KEYWORD None)
(PUNCT ",")
(ID "parent")
(PUNCT "=")
(KEYWORD None)
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(KEYWORD not)
(ID "parent")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "parent")
(PUNCT "=")
(ID "tkinter")
(PUNCT ".")
(ID "_default_root")
(NEWLINE)
(DEDENT)
(ID "self")
(PUNCT ".")
(ID "prompt")
(PUNCT "=")
(ID "prompt")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "minvalue")
(PUNCT "=")
(ID "minvalue")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "maxvalue")
(PUNCT "=")
(ID "maxvalue")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "initialvalue")
(PUNCT "=")
(ID "initialvalue")
(NEWLINE)
(ID "Dialog")
(PUNCT ".")
(ID "__init__")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "parent")
(PUNCT ",")
(ID "title")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "destroy")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "entry")
(PUNCT "=")
(KEYWORD None)
(NEWLINE)
(ID "Dialog")
(PUNCT ".")
(ID "destroy")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "body")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "master")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "w")
(PUNCT "=")
(ID "Label")
(PUNCT "(")
(ID "master")
(PUNCT ",")
(ID "text")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "prompt")
(PUNCT ",")
(ID "justify")
(PUNCT "=")
(ID "LEFT")
(PUNCT ")")
(NEWLINE)
(ID "w")
(PUNCT ".")
(ID "grid")
(PUNCT "(")
(ID "row")
(PUNCT "=")
(LIT 0)
(PUNCT ",")
(ID "padx")
(PUNCT "=")
(LIT 5)
(PUNCT ",")
(ID "sticky")
(PUNCT "=")
(ID "W")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "entry")
(PUNCT "=")
(ID "Entry")
(PUNCT "(")
(ID "master")
(PUNCT ",")
(ID "name")
(PUNCT "=")
(LIT "entry")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "entry")
(PUNCT ".")
(ID "grid")
(PUNCT "(")
(ID "row")
(PUNCT "=")
(LIT 1)
(PUNCT ",")
(ID "padx")
(PUNCT "=")
(LIT 5)
(PUNCT ",")
(ID "sticky")
(PUNCT "=")
(ID "W")
(PUNCT "+")
(ID "E")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(ID "self")
(PUNCT ".")
(ID "initialvalue")
(KEYWORD is)
(KEYWORD not)
(KEYWORD None)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "entry")
(PUNCT ".")
(ID "insert")
(PUNCT "(")
(LIT 0)
(PUNCT ",")
(ID "self")
(PUNCT ".")
(ID "initialvalue")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "entry")
(PUNCT ".")
(ID "select_range")
(PUNCT "(")
(LIT 0)
(PUNCT ",")
(ID "END")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD return)
(ID "self")
(PUNCT ".")
(ID "entry")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "validate")
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
(ID "result")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "getresult")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD except)
(ID "ValueError")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "messagebox")
(PUNCT ".")
(ID "showwarning")
(PUNCT "(")
(LIT "Illegal value")
(PUNCT ",")
(ID "self")
(PUNCT ".")
(ID "errormessage")
(PUNCT "+")
(LIT "\nPlease try again")
(PUNCT ",")
(ID "parent")
(PUNCT "=")
(ID "self")
(PUNCT ")")
(NEWLINE)
(KEYWORD return)
(LIT 0)
(NEWLINE)
(DEDENT)
(KEYWORD if)
(ID "self")
(PUNCT ".")
(ID "minvalue")
(KEYWORD is)
(KEYWORD not)
(KEYWORD None)
(KEYWORD and)
(ID "result")
(PUNCT "<")
(ID "self")
(PUNCT ".")
(ID "minvalue")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "messagebox")
(PUNCT ".")
(ID "showwarning")
(PUNCT "(")
(LIT "Too small")
(PUNCT ",")
(LIT "The allowed minimum value is %s. ")
(LIT "Please try again.")
(PUNCT "%")
(ID "self")
(PUNCT ".")
(ID "minvalue")
(PUNCT ",")
(ID "parent")
(PUNCT "=")
(ID "self")
(PUNCT ")")
(NEWLINE)
(KEYWORD return)
(LIT 0)
(NEWLINE)
(DEDENT)
(KEYWORD if)
(ID "self")
(PUNCT ".")
(ID "maxvalue")
(KEYWORD is)
(KEYWORD not)
(KEYWORD None)
(KEYWORD and)
(ID "result")
(PUNCT ">")
(ID "self")
(PUNCT ".")
(ID "maxvalue")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "messagebox")
(PUNCT ".")
(ID "showwarning")
(PUNCT "(")
(LIT "Too large")
(PUNCT ",")
(LIT "The allowed maximum value is %s. ")
(LIT "Please try again.")
(PUNCT "%")
(ID "self")
(PUNCT ".")
(ID "maxvalue")
(PUNCT ",")
(ID "parent")
(PUNCT "=")
(ID "self")
(PUNCT ")")
(NEWLINE)
(KEYWORD return)
(LIT 0)
(NEWLINE)
(DEDENT)
(ID "self")
(PUNCT ".")
(ID "result")
(PUNCT "=")
(ID "result")
(NEWLINE)
(KEYWORD return)
(LIT 1)
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD class)
(ID "_QueryInteger")
(PUNCT "(")
(ID "_QueryDialog")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "errormessage")
(PUNCT "=")
(LIT "Not an integer.")
(NEWLINE)
(KEYWORD def)
(ID "getresult")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(ID "int")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "entry")
(PUNCT ".")
(ID "get")
(PUNCT "(")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD def)
(ID "askinteger")
(PUNCT "(")
(ID "title")
(PUNCT ",")
(ID "prompt")
(PUNCT ",")
(PUNCT "**")
(ID "kw")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "get an integer from the user\n\n    Arguments:\n\n        title -- the dialog title\n        prompt -- the label text\n        **kw -- see SimpleDialog class\n\n    Return value is an integer\n    ")
(NEWLINE)
(ID "d")
(PUNCT "=")
(ID "_QueryInteger")
(PUNCT "(")
(ID "title")
(PUNCT ",")
(ID "prompt")
(PUNCT ",")
(PUNCT "**")
(ID "kw")
(PUNCT ")")
(NEWLINE)
(KEYWORD return)
(ID "d")
(PUNCT ".")
(ID "result")
(NEWLINE)
(DEDENT)
(KEYWORD class)
(ID "_QueryFloat")
(PUNCT "(")
(ID "_QueryDialog")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "errormessage")
(PUNCT "=")
(LIT "Not a floating point value.")
(NEWLINE)
(KEYWORD def)
(ID "getresult")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(ID "float")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "entry")
(PUNCT ".")
(ID "get")
(PUNCT "(")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD def)
(ID "askfloat")
(PUNCT "(")
(ID "title")
(PUNCT ",")
(ID "prompt")
(PUNCT ",")
(PUNCT "**")
(ID "kw")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "get a float from the user\n\n    Arguments:\n\n        title -- the dialog title\n        prompt -- the label text\n        **kw -- see SimpleDialog class\n\n    Return value is a float\n    ")
(NEWLINE)
(ID "d")
(PUNCT "=")
(ID "_QueryFloat")
(PUNCT "(")
(ID "title")
(PUNCT ",")
(ID "prompt")
(PUNCT ",")
(PUNCT "**")
(ID "kw")
(PUNCT ")")
(NEWLINE)
(KEYWORD return)
(ID "d")
(PUNCT ".")
(ID "result")
(NEWLINE)
(DEDENT)
(KEYWORD class)
(ID "_QueryString")
(PUNCT "(")
(ID "_QueryDialog")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD def)
(ID "__init__")
(PUNCT "(")
(ID "self")
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
(KEYWORD if)
(LIT "show")
(KEYWORD in)
(ID "kw")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "__show")
(PUNCT "=")
(ID "kw")
(PUNCT "[")
(LIT "show")
(PUNCT "]")
(NEWLINE)
(KEYWORD del)
(ID "kw")
(PUNCT "[")
(LIT "show")
(PUNCT "]")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "__show")
(PUNCT "=")
(KEYWORD None)
(NEWLINE)
(DEDENT)
(ID "_QueryDialog")
(PUNCT ".")
(ID "__init__")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(PUNCT "*")
(ID "args")
(PUNCT ",")
(PUNCT "**")
(ID "kw")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "body")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "master")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "entry")
(PUNCT "=")
(ID "_QueryDialog")
(PUNCT ".")
(ID "body")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "master")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(ID "self")
(PUNCT ".")
(ID "__show")
(KEYWORD is)
(KEYWORD not)
(KEYWORD None)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "entry")
(PUNCT ".")
(ID "configure")
(PUNCT "(")
(ID "show")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "__show")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD return)
(ID "entry")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "getresult")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(ID "self")
(PUNCT ".")
(ID "entry")
(PUNCT ".")
(ID "get")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD def)
(ID "askstring")
(PUNCT "(")
(ID "title")
(PUNCT ",")
(ID "prompt")
(PUNCT ",")
(PUNCT "**")
(ID "kw")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "get a string from the user\n\n    Arguments:\n\n        title -- the dialog title\n        prompt -- the label text\n        **kw -- see SimpleDialog class\n\n    Return value is a string\n    ")
(NEWLINE)
(ID "d")
(PUNCT "=")
(ID "_QueryString")
(PUNCT "(")
(ID "title")
(PUNCT ",")
(ID "prompt")
(PUNCT ",")
(PUNCT "**")
(ID "kw")
(PUNCT ")")
(NEWLINE)
(KEYWORD return)
(ID "d")
(PUNCT ".")
(ID "result")
(NEWLINE)
(DEDENT)
(KEYWORD if)
(ID "__name__")
(PUNCT "==")
(LIT "__main__")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD def)
(ID "test")
(PUNCT "(")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "root")
(PUNCT "=")
(ID "Tk")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(KEYWORD def)
(ID "doit")
(PUNCT "(")
(ID "root")
(PUNCT "=")
(ID "root")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "d")
(PUNCT "=")
(ID "SimpleDialog")
(PUNCT "(")
(ID "root")
(PUNCT ",")
(ID "text")
(PUNCT "=")
(LIT "This is a test dialog.  ")
(LIT "Would this have been an actual dialog, ")
(LIT "the buttons below would have been glowing ")
(LIT "in soft pink light.\n")
(LIT "Do you believe this?")
(PUNCT ",")
(ID "buttons")
(PUNCT "=")
(PUNCT "[")
(LIT "Yes")
(PUNCT ",")
(LIT "No")
(PUNCT ",")
(LIT "Cancel")
(PUNCT "]")
(PUNCT ",")
(ID "default")
(PUNCT "=")
(LIT 0)
(PUNCT ",")
(ID "cancel")
(PUNCT "=")
(LIT 2)
(PUNCT ",")
(ID "title")
(PUNCT "=")
(LIT "Test Dialog")
(PUNCT ")")
(NEWLINE)
(ID "print")
(PUNCT "(")
(ID "d")
(PUNCT ".")
(ID "go")
(PUNCT "(")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(ID "print")
(PUNCT "(")
(ID "askinteger")
(PUNCT "(")
(LIT "Spam")
(PUNCT ",")
(LIT "Egg count")
(PUNCT ",")
(ID "initialvalue")
(PUNCT "=")
(LIT 12)
(PUNCT "*")
(LIT 12)
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(ID "print")
(PUNCT "(")
(ID "askfloat")
(PUNCT "(")
(LIT "Spam")
(PUNCT ",")
(LIT "Egg weight\n(in tons)")
(PUNCT ",")
(ID "minvalue")
(PUNCT "=")
(LIT 1)
(PUNCT ",")
(ID "maxvalue")
(PUNCT "=")
(LIT 100)
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(ID "print")
(PUNCT "(")
(ID "askstring")
(PUNCT "(")
(LIT "Spam")
(PUNCT ",")
(LIT "Egg label")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "t")
(PUNCT "=")
(ID "Button")
(PUNCT "(")
(ID "root")
(PUNCT ",")
(ID "text")
(PUNCT "=")
(LIT "Test")
(PUNCT ",")
(ID "command")
(PUNCT "=")
(ID "doit")
(PUNCT ")")
(NEWLINE)
(ID "t")
(PUNCT ".")
(ID "pack")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "q")
(PUNCT "=")
(ID "Button")
(PUNCT "(")
(ID "root")
(PUNCT ",")
(ID "text")
(PUNCT "=")
(LIT "Quit")
(PUNCT ",")
(ID "command")
(PUNCT "=")
(ID "t")
(PUNCT ".")
(ID "quit")
(PUNCT ")")
(NEWLINE)
(ID "q")
(PUNCT ".")
(ID "pack")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "t")
(PUNCT ".")
(ID "mainloop")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "test")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ENDMARKER)
