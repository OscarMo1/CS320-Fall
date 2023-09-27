def fnlist_make_fwork(fwork):
    res = []

    def work(x0):
        res.append(x0)

    fwork(work)
    return list(reversed(res))


