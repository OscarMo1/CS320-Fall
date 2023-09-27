from MyPython import *

def fnlist_make_fwork(fwork):
    res = fnlist_nil()
    
    def work(x0):
        nonlocal res
        res = fnlist_cons(x0, res)
    
    fwork(work)
    return fnlist_reverse(res)





