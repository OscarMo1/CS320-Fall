def generator_of_stream(fxs):
    while True:
        cxs = fxs()
        if cxs[0] == 0:
            break
        else:
            fxs = cxs[1]
            yield cxs[2]
    raise StopIteration

def theNatPairs_cubesum():
    def cube_sum(i, j):
        return i**3 + j**3

    def pairs_generator():
        i, j = 0, 0
        while True:
            yield (i, j)
            if j == 0:
                i += 1
                j = i
            else:
                j -= 1

    def compare_pairs(pair1, pair2):
        i1, j1 = pair1
        i2, j2 = pair2
        return cube_sum(i1, j1) < cube_sum(i2, j2)

    sorted_pairs = sorted(pairs_generator(), key=lambda pair: cube_sum(pair[0], pair[1]))

    for pair in sorted_pairs:
        yield pair



