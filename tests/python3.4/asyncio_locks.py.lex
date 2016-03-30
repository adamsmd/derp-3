(LIT "Synchronization primitives.")
(NEWLINE)
(ID "__all__")
(PUNCT "=")
(PUNCT "[")
(LIT "Lock")
(PUNCT ",")
(LIT "Event")
(PUNCT ",")
(LIT "Condition")
(PUNCT ",")
(LIT "Semaphore")
(PUNCT ",")
(LIT "BoundedSemaphore")
(PUNCT "]")
(NEWLINE)
(KEYWORD import)
(ID "collections")
(NEWLINE)
(KEYWORD from)
(PUNCT ".")
(KEYWORD import)
(ID "events")
(NEWLINE)
(KEYWORD from)
(PUNCT ".")
(KEYWORD import)
(ID "futures")
(NEWLINE)
(KEYWORD from)
(PUNCT ".")
(ID "coroutines")
(KEYWORD import)
(ID "coroutine")
(NEWLINE)
(KEYWORD class)
(ID "_ContextManager")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Context manager.\n\n    This enables the following idiom for acquiring and releasing a\n    lock around a block:\n\n        with (yield from lock):\n            <block>\n\n    while failing loudly when accidentally using:\n\n        with lock:\n            <block>\n    ")
(NEWLINE)
(KEYWORD def)
(ID "__init__")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "lock")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "_lock")
(PUNCT "=")
(ID "lock")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "__enter__")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(KEYWORD None)
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "__exit__")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(PUNCT "*")
(ID "args")
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
(ID "_lock")
(PUNCT ".")
(ID "release")
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
(ID "_lock")
(PUNCT "=")
(KEYWORD None)
(NEWLINE)
(DEDENT)
(DEDENT)
(DEDENT)
(KEYWORD class)
(ID "Lock")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Primitive lock objects.\n\n    A primitive lock is a synchronization primitive that is not owned\n    by a particular coroutine when locked.  A primitive lock is in one\n    of two states, 'locked' or 'unlocked'.\n\n    It is created in the unlocked state.  It has two basic methods,\n    acquire() and release().  When the state is unlocked, acquire()\n    changes the state to locked and returns immediately.  When the\n    state is locked, acquire() blocks until a call to release() in\n    another coroutine changes it to unlocked, then the acquire() call\n    resets it to locked and returns.  The release() method should only\n    be called in the locked state; it changes the state to unlocked\n    and returns immediately.  If an attempt is made to release an\n    unlocked lock, a RuntimeError will be raised.\n\n    When more than one coroutine is blocked in acquire() waiting for\n    the state to turn to unlocked, only one coroutine proceeds when a\n    release() call resets the state to unlocked; first coroutine which\n    is blocked in acquire() is being processed.\n\n    acquire() is a coroutine and should be called with 'yield from'.\n\n    Locks also support the context management protocol.  '(yield from lock)'\n    should be used as context manager expression.\n\n    Usage:\n\n        lock = Lock()\n        ...\n        yield from lock\n        try:\n            ...\n        finally:\n            lock.release()\n\n    Context manager usage:\n\n        lock = Lock()\n        ...\n        with (yield from lock):\n             ...\n\n    Lock objects can be tested for locking state:\n\n        if not lock.locked():\n           yield from lock\n        else:\n           # lock is acquired\n           ...\n\n    ")
(NEWLINE)
(KEYWORD def)
(ID "__init__")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(PUNCT "*")
(PUNCT ",")
(ID "loop")
(PUNCT "=")
(KEYWORD None)
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "_waiters")
(PUNCT "=")
(ID "collections")
(PUNCT ".")
(ID "deque")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_locked")
(PUNCT "=")
(KEYWORD False)
(NEWLINE)
(KEYWORD if)
(ID "loop")
(KEYWORD is)
(KEYWORD not)
(KEYWORD None)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "_loop")
(PUNCT "=")
(ID "loop")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "_loop")
(PUNCT "=")
(ID "events")
(PUNCT ".")
(ID "get_event_loop")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD def)
(ID "__repr__")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "res")
(PUNCT "=")
(ID "super")
(PUNCT "(")
(PUNCT ")")
(PUNCT ".")
(ID "__repr__")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "extra")
(PUNCT "=")
(LIT "locked")
(KEYWORD if)
(ID "self")
(PUNCT ".")
(ID "_locked")
(KEYWORD else)
(LIT "unlocked")
(NEWLINE)
(KEYWORD if)
(ID "self")
(PUNCT ".")
(ID "_waiters")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "extra")
(PUNCT "=")
(LIT "{},waiters:{}")
(PUNCT ".")
(ID "format")
(PUNCT "(")
(ID "extra")
(PUNCT ",")
(ID "len")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "_waiters")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD return)
(LIT "<{} [{}]>")
(PUNCT ".")
(ID "format")
(PUNCT "(")
(ID "res")
(PUNCT "[")
(LIT 1)
(PUNCT ":")
(PUNCT "-")
(LIT 1)
(PUNCT "]")
(PUNCT ",")
(ID "extra")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "locked")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Return True if lock is acquired.")
(NEWLINE)
(KEYWORD return)
(ID "self")
(PUNCT ".")
(ID "_locked")
(NEWLINE)
(DEDENT)
(PUNCT "@")
(ID "coroutine")
(NEWLINE)
(KEYWORD def)
(ID "acquire")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Acquire a lock.\n\n        This method blocks until the lock is unlocked, then sets it to\n        locked and returns True.\n        ")
(NEWLINE)
(KEYWORD if)
(KEYWORD not)
(ID "self")
(PUNCT ".")
(ID "_waiters")
(KEYWORD and)
(KEYWORD not)
(ID "self")
(PUNCT ".")
(ID "_locked")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "_locked")
(PUNCT "=")
(KEYWORD True)
(NEWLINE)
(KEYWORD return)
(KEYWORD True)
(NEWLINE)
(DEDENT)
(ID "fut")
(PUNCT "=")
(ID "futures")
(PUNCT ".")
(ID "Future")
(PUNCT "(")
(ID "loop")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "_loop")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_waiters")
(PUNCT ".")
(ID "append")
(PUNCT "(")
(ID "fut")
(PUNCT ")")
(NEWLINE)
(KEYWORD try)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD yield)
(KEYWORD from)
(ID "fut")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_locked")
(PUNCT "=")
(KEYWORD True)
(NEWLINE)
(KEYWORD return)
(KEYWORD True)
(NEWLINE)
(DEDENT)
(KEYWORD finally)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "_waiters")
(PUNCT ".")
(ID "remove")
(PUNCT "(")
(ID "fut")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD def)
(ID "release")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Release a lock.\n\n        When the lock is locked, reset it to unlocked, and return.\n        If any other coroutines are blocked waiting for the lock to become\n        unlocked, allow exactly one of them to proceed.\n\n        When invoked on an unlocked lock, a RuntimeError is raised.\n\n        There is no return value.\n        ")
(NEWLINE)
(KEYWORD if)
(ID "self")
(PUNCT ".")
(ID "_locked")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "_locked")
(PUNCT "=")
(KEYWORD False)
(NEWLINE)
(KEYWORD for)
(ID "fut")
(KEYWORD in)
(ID "self")
(PUNCT ".")
(ID "_waiters")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(KEYWORD not)
(ID "fut")
(PUNCT ".")
(ID "done")
(PUNCT "(")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "fut")
(PUNCT ".")
(ID "set_result")
(PUNCT "(")
(KEYWORD True)
(PUNCT ")")
(NEWLINE)
(KEYWORD break)
(NEWLINE)
(DEDENT)
(DEDENT)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD raise)
(ID "RuntimeError")
(PUNCT "(")
(LIT "Lock is not acquired.")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD def)
(ID "__enter__")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD raise)
(ID "RuntimeError")
(PUNCT "(")
(LIT "\"yield from\" should be used as context manager expression")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "__exit__")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(PUNCT "*")
(ID "args")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD pass)
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "__iter__")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD yield)
(KEYWORD from)
(ID "self")
(PUNCT ".")
(ID "acquire")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(KEYWORD return)
(ID "_ContextManager")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD class)
(ID "Event")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Asynchronous equivalent to threading.Event.\n\n    Class implementing event objects. An event manages a flag that can be set\n    to true with the set() method and reset to false with the clear() method.\n    The wait() method blocks until the flag is true. The flag is initially\n    false.\n    ")
(NEWLINE)
(KEYWORD def)
(ID "__init__")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(PUNCT "*")
(PUNCT ",")
(ID "loop")
(PUNCT "=")
(KEYWORD None)
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "_waiters")
(PUNCT "=")
(ID "collections")
(PUNCT ".")
(ID "deque")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_value")
(PUNCT "=")
(KEYWORD False)
(NEWLINE)
(KEYWORD if)
(ID "loop")
(KEYWORD is)
(KEYWORD not)
(KEYWORD None)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "_loop")
(PUNCT "=")
(ID "loop")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "_loop")
(PUNCT "=")
(ID "events")
(PUNCT ".")
(ID "get_event_loop")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD def)
(ID "__repr__")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "res")
(PUNCT "=")
(ID "super")
(PUNCT "(")
(PUNCT ")")
(PUNCT ".")
(ID "__repr__")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "extra")
(PUNCT "=")
(LIT "set")
(KEYWORD if)
(ID "self")
(PUNCT ".")
(ID "_value")
(KEYWORD else)
(LIT "unset")
(NEWLINE)
(KEYWORD if)
(ID "self")
(PUNCT ".")
(ID "_waiters")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "extra")
(PUNCT "=")
(LIT "{},waiters:{}")
(PUNCT ".")
(ID "format")
(PUNCT "(")
(ID "extra")
(PUNCT ",")
(ID "len")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "_waiters")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD return)
(LIT "<{} [{}]>")
(PUNCT ".")
(ID "format")
(PUNCT "(")
(ID "res")
(PUNCT "[")
(LIT 1)
(PUNCT ":")
(PUNCT "-")
(LIT 1)
(PUNCT "]")
(PUNCT ",")
(ID "extra")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "is_set")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Return True if and only if the internal flag is true.")
(NEWLINE)
(KEYWORD return)
(ID "self")
(PUNCT ".")
(ID "_value")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "set")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Set the internal flag to true. All coroutines waiting for it to\n        become true are awakened. Coroutine that call wait() once the flag is\n        true will not block at all.\n        ")
(NEWLINE)
(KEYWORD if)
(KEYWORD not)
(ID "self")
(PUNCT ".")
(ID "_value")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "_value")
(PUNCT "=")
(KEYWORD True)
(NEWLINE)
(KEYWORD for)
(ID "fut")
(KEYWORD in)
(ID "self")
(PUNCT ".")
(ID "_waiters")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(KEYWORD not)
(ID "fut")
(PUNCT ".")
(ID "done")
(PUNCT "(")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "fut")
(PUNCT ".")
(ID "set_result")
(PUNCT "(")
(KEYWORD True)
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(DEDENT)
(DEDENT)
(KEYWORD def)
(ID "clear")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Reset the internal flag to false. Subsequently, coroutines calling\n        wait() will block until set() is called to set the internal flag\n        to true again.")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_value")
(PUNCT "=")
(KEYWORD False)
(NEWLINE)
(DEDENT)
(PUNCT "@")
(ID "coroutine")
(NEWLINE)
(KEYWORD def)
(ID "wait")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Block until the internal flag is true.\n\n        If the internal flag is true on entry, return True\n        immediately.  Otherwise, block until another coroutine calls\n        set() to set the flag to true, then return True.\n        ")
(NEWLINE)
(KEYWORD if)
(ID "self")
(PUNCT ".")
(ID "_value")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(KEYWORD True)
(NEWLINE)
(DEDENT)
(ID "fut")
(PUNCT "=")
(ID "futures")
(PUNCT ".")
(ID "Future")
(PUNCT "(")
(ID "loop")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "_loop")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_waiters")
(PUNCT ".")
(ID "append")
(PUNCT "(")
(ID "fut")
(PUNCT ")")
(NEWLINE)
(KEYWORD try)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD yield)
(KEYWORD from)
(ID "fut")
(NEWLINE)
(KEYWORD return)
(KEYWORD True)
(NEWLINE)
(DEDENT)
(KEYWORD finally)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "_waiters")
(PUNCT ".")
(ID "remove")
(PUNCT "(")
(ID "fut")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(DEDENT)
(KEYWORD class)
(ID "Condition")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Asynchronous equivalent to threading.Condition.\n\n    This class implements condition variable objects. A condition variable\n    allows one or more coroutines to wait until they are notified by another\n    coroutine.\n\n    A new Lock object is created and used as the underlying lock.\n    ")
(NEWLINE)
(KEYWORD def)
(ID "__init__")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "lock")
(PUNCT "=")
(KEYWORD None)
(PUNCT ",")
(PUNCT "*")
(PUNCT ",")
(ID "loop")
(PUNCT "=")
(KEYWORD None)
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "loop")
(KEYWORD is)
(KEYWORD not)
(KEYWORD None)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "_loop")
(PUNCT "=")
(ID "loop")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "_loop")
(PUNCT "=")
(ID "events")
(PUNCT ".")
(ID "get_event_loop")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD if)
(ID "lock")
(KEYWORD is)
(KEYWORD None)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "lock")
(PUNCT "=")
(ID "Lock")
(PUNCT "(")
(ID "loop")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "_loop")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD elif)
(ID "lock")
(PUNCT ".")
(ID "_loop")
(KEYWORD is)
(KEYWORD not)
(ID "self")
(PUNCT ".")
(ID "_loop")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD raise)
(ID "ValueError")
(PUNCT "(")
(LIT "loop argument must agree with lock")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "self")
(PUNCT ".")
(ID "_lock")
(PUNCT "=")
(ID "lock")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "locked")
(PUNCT "=")
(ID "lock")
(PUNCT ".")
(ID "locked")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "acquire")
(PUNCT "=")
(ID "lock")
(PUNCT ".")
(ID "acquire")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "release")
(PUNCT "=")
(ID "lock")
(PUNCT ".")
(ID "release")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_waiters")
(PUNCT "=")
(ID "collections")
(PUNCT ".")
(ID "deque")
(PUNCT "(")
(PUNCT ")")
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
(ID "res")
(PUNCT "=")
(ID "super")
(PUNCT "(")
(PUNCT ")")
(PUNCT ".")
(ID "__repr__")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "extra")
(PUNCT "=")
(LIT "locked")
(KEYWORD if)
(ID "self")
(PUNCT ".")
(ID "locked")
(PUNCT "(")
(PUNCT ")")
(KEYWORD else)
(LIT "unlocked")
(NEWLINE)
(KEYWORD if)
(ID "self")
(PUNCT ".")
(ID "_waiters")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "extra")
(PUNCT "=")
(LIT "{},waiters:{}")
(PUNCT ".")
(ID "format")
(PUNCT "(")
(ID "extra")
(PUNCT ",")
(ID "len")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "_waiters")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD return)
(LIT "<{} [{}]>")
(PUNCT ".")
(ID "format")
(PUNCT "(")
(ID "res")
(PUNCT "[")
(LIT 1)
(PUNCT ":")
(PUNCT "-")
(LIT 1)
(PUNCT "]")
(PUNCT ",")
(ID "extra")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(PUNCT "@")
(ID "coroutine")
(NEWLINE)
(KEYWORD def)
(ID "wait")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Wait until notified.\n\n        If the calling coroutine has not acquired the lock when this\n        method is called, a RuntimeError is raised.\n\n        This method releases the underlying lock, and then blocks\n        until it is awakened by a notify() or notify_all() call for\n        the same condition variable in another coroutine.  Once\n        awakened, it re-acquires the lock and returns True.\n        ")
(NEWLINE)
(KEYWORD if)
(KEYWORD not)
(ID "self")
(PUNCT ".")
(ID "locked")
(PUNCT "(")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD raise)
(ID "RuntimeError")
(PUNCT "(")
(LIT "cannot wait on un-acquired lock")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "self")
(PUNCT ".")
(ID "release")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(KEYWORD try)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "fut")
(PUNCT "=")
(ID "futures")
(PUNCT ".")
(ID "Future")
(PUNCT "(")
(ID "loop")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "_loop")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_waiters")
(PUNCT ".")
(ID "append")
(PUNCT "(")
(ID "fut")
(PUNCT ")")
(NEWLINE)
(KEYWORD try)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD yield)
(KEYWORD from)
(ID "fut")
(NEWLINE)
(KEYWORD return)
(KEYWORD True)
(NEWLINE)
(DEDENT)
(KEYWORD finally)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "_waiters")
(PUNCT ".")
(ID "remove")
(PUNCT "(")
(ID "fut")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD finally)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD yield)
(KEYWORD from)
(ID "self")
(PUNCT ".")
(ID "acquire")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(PUNCT "@")
(ID "coroutine")
(NEWLINE)
(KEYWORD def)
(ID "wait_for")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "predicate")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Wait until a predicate becomes true.\n\n        The predicate should be a callable which result will be\n        interpreted as a boolean value.  The final predicate value is\n        the return value.\n        ")
(NEWLINE)
(ID "result")
(PUNCT "=")
(ID "predicate")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(KEYWORD while)
(KEYWORD not)
(ID "result")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD yield)
(KEYWORD from)
(ID "self")
(PUNCT ".")
(ID "wait")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "result")
(PUNCT "=")
(ID "predicate")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD return)
(ID "result")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "notify")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "n")
(PUNCT "=")
(LIT 1)
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "By default, wake up one coroutine waiting on this condition, if any.\n        If the calling coroutine has not acquired the lock when this method\n        is called, a RuntimeError is raised.\n\n        This method wakes up at most n of the coroutines waiting for the\n        condition variable; it is a no-op if no coroutines are waiting.\n\n        Note: an awakened coroutine does not actually return from its\n        wait() call until it can reacquire the lock. Since notify() does\n        not release the lock, its caller should.\n        ")
(NEWLINE)
(KEYWORD if)
(KEYWORD not)
(ID "self")
(PUNCT ".")
(ID "locked")
(PUNCT "(")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD raise)
(ID "RuntimeError")
(PUNCT "(")
(LIT "cannot notify on un-acquired lock")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "idx")
(PUNCT "=")
(LIT 0)
(NEWLINE)
(KEYWORD for)
(ID "fut")
(KEYWORD in)
(ID "self")
(PUNCT ".")
(ID "_waiters")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "idx")
(PUNCT ">=")
(ID "n")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD break)
(NEWLINE)
(DEDENT)
(KEYWORD if)
(KEYWORD not)
(ID "fut")
(PUNCT ".")
(ID "done")
(PUNCT "(")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "idx")
(PUNCT "+=")
(LIT 1)
(NEWLINE)
(ID "fut")
(PUNCT ".")
(ID "set_result")
(PUNCT "(")
(KEYWORD False)
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(DEDENT)
(KEYWORD def)
(ID "notify_all")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Wake up all threads waiting on this condition. This method acts\n        like notify(), but wakes up all waiting threads instead of one. If the\n        calling thread has not acquired the lock when this method is called,\n        a RuntimeError is raised.\n        ")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "notify")
(PUNCT "(")
(ID "len")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "_waiters")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "__enter__")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD raise)
(ID "RuntimeError")
(PUNCT "(")
(LIT "\"yield from\" should be used as context manager expression")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "__exit__")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(PUNCT "*")
(ID "args")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD pass)
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "__iter__")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD yield)
(KEYWORD from)
(ID "self")
(PUNCT ".")
(ID "acquire")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(KEYWORD return)
(ID "_ContextManager")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD class)
(ID "Semaphore")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "A Semaphore implementation.\n\n    A semaphore manages an internal counter which is decremented by each\n    acquire() call and incremented by each release() call. The counter\n    can never go below zero; when acquire() finds that it is zero, it blocks,\n    waiting until some other thread calls release().\n\n    Semaphores also support the context management protocol.\n\n    The optional argument gives the initial value for the internal\n    counter; it defaults to 1. If the value given is less than 0,\n    ValueError is raised.\n    ")
(NEWLINE)
(KEYWORD def)
(ID "__init__")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "value")
(PUNCT "=")
(LIT 1)
(PUNCT ",")
(PUNCT "*")
(PUNCT ",")
(ID "loop")
(PUNCT "=")
(KEYWORD None)
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "value")
(PUNCT "<")
(LIT 0)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD raise)
(ID "ValueError")
(PUNCT "(")
(LIT "Semaphore initial value must be >= 0")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "self")
(PUNCT ".")
(ID "_value")
(PUNCT "=")
(ID "value")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_waiters")
(PUNCT "=")
(ID "collections")
(PUNCT ".")
(ID "deque")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(ID "loop")
(KEYWORD is)
(KEYWORD not)
(KEYWORD None)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "_loop")
(PUNCT "=")
(ID "loop")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "_loop")
(PUNCT "=")
(ID "events")
(PUNCT ".")
(ID "get_event_loop")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD def)
(ID "__repr__")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "res")
(PUNCT "=")
(ID "super")
(PUNCT "(")
(PUNCT ")")
(PUNCT ".")
(ID "__repr__")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(ID "extra")
(PUNCT "=")
(LIT "locked")
(KEYWORD if)
(ID "self")
(PUNCT ".")
(ID "locked")
(PUNCT "(")
(PUNCT ")")
(KEYWORD else)
(LIT "unlocked,value:{}")
(PUNCT ".")
(ID "format")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "_value")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(ID "self")
(PUNCT ".")
(ID "_waiters")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "extra")
(PUNCT "=")
(LIT "{},waiters:{}")
(PUNCT ".")
(ID "format")
(PUNCT "(")
(ID "extra")
(PUNCT ",")
(ID "len")
(PUNCT "(")
(ID "self")
(PUNCT ".")
(ID "_waiters")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD return)
(LIT "<{} [{}]>")
(PUNCT ".")
(ID "format")
(PUNCT "(")
(ID "res")
(PUNCT "[")
(LIT 1)
(PUNCT ":")
(PUNCT "-")
(LIT 1)
(PUNCT "]")
(PUNCT ",")
(ID "extra")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "locked")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Returns True if semaphore can not be acquired immediately.")
(NEWLINE)
(KEYWORD return)
(ID "self")
(PUNCT ".")
(ID "_value")
(PUNCT "==")
(LIT 0)
(NEWLINE)
(DEDENT)
(PUNCT "@")
(ID "coroutine")
(NEWLINE)
(KEYWORD def)
(ID "acquire")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Acquire a semaphore.\n\n        If the internal counter is larger than zero on entry,\n        decrement it by one and return True immediately.  If it is\n        zero on entry, block, waiting until some other coroutine has\n        called release() to make it larger than 0, and then return\n        True.\n        ")
(NEWLINE)
(KEYWORD if)
(KEYWORD not)
(ID "self")
(PUNCT ".")
(ID "_waiters")
(KEYWORD and)
(ID "self")
(PUNCT ".")
(ID "_value")
(PUNCT ">")
(LIT 0)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "_value")
(PUNCT "-=")
(LIT 1)
(NEWLINE)
(KEYWORD return)
(KEYWORD True)
(NEWLINE)
(DEDENT)
(ID "fut")
(PUNCT "=")
(ID "futures")
(PUNCT ".")
(ID "Future")
(PUNCT "(")
(ID "loop")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "_loop")
(PUNCT ")")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_waiters")
(PUNCT ".")
(ID "append")
(PUNCT "(")
(ID "fut")
(PUNCT ")")
(NEWLINE)
(KEYWORD try)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD yield)
(KEYWORD from)
(ID "fut")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_value")
(PUNCT "-=")
(LIT 1)
(NEWLINE)
(KEYWORD return)
(KEYWORD True)
(NEWLINE)
(DEDENT)
(KEYWORD finally)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "_waiters")
(PUNCT ".")
(ID "remove")
(PUNCT "(")
(ID "fut")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD def)
(ID "release")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "Release a semaphore, incrementing the internal counter by one.\n        When it was zero on entry and another coroutine is waiting for it to\n        become larger than zero again, wake up that coroutine.\n        ")
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "_value")
(PUNCT "+=")
(LIT 1)
(NEWLINE)
(KEYWORD for)
(ID "waiter")
(KEYWORD in)
(ID "self")
(PUNCT ".")
(ID "_waiters")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(KEYWORD not)
(ID "waiter")
(PUNCT ".")
(ID "done")
(PUNCT "(")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "waiter")
(PUNCT ".")
(ID "set_result")
(PUNCT "(")
(KEYWORD True)
(PUNCT ")")
(NEWLINE)
(KEYWORD break)
(NEWLINE)
(DEDENT)
(DEDENT)
(DEDENT)
(KEYWORD def)
(ID "__enter__")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD raise)
(ID "RuntimeError")
(PUNCT "(")
(LIT "\"yield from\" should be used as context manager expression")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "__exit__")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(PUNCT "*")
(ID "args")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD pass)
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "__iter__")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD yield)
(KEYWORD from)
(ID "self")
(PUNCT ".")
(ID "acquire")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(KEYWORD return)
(ID "_ContextManager")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD class)
(ID "BoundedSemaphore")
(PUNCT "(")
(ID "Semaphore")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "A bounded semaphore implementation.\n\n    This raises ValueError in release() if it would increase the value\n    above the initial value.\n    ")
(NEWLINE)
(KEYWORD def)
(ID "__init__")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "value")
(PUNCT "=")
(LIT 1)
(PUNCT ",")
(PUNCT "*")
(PUNCT ",")
(ID "loop")
(PUNCT "=")
(KEYWORD None)
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "_bound_value")
(PUNCT "=")
(ID "value")
(NEWLINE)
(ID "super")
(PUNCT "(")
(PUNCT ")")
(PUNCT ".")
(ID "__init__")
(PUNCT "(")
(ID "value")
(PUNCT ",")
(ID "loop")
(PUNCT "=")
(ID "loop")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "release")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "self")
(PUNCT ".")
(ID "_value")
(PUNCT ">=")
(ID "self")
(PUNCT ".")
(ID "_bound_value")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD raise)
(ID "ValueError")
(PUNCT "(")
(LIT "BoundedSemaphore released too many times")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ID "super")
(PUNCT "(")
(PUNCT ")")
(PUNCT ".")
(ID "release")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(ENDMARKER)
