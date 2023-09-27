def fnlist_make_fwork(fwork):
    result = []
    
    def work(x):
        result.append(x)
    
    fwork(work)
    return result



