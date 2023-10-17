def theNatPairs_cubesum():
    def cube_sum(i, j):
        return i**3 + j**3

    i, j = 0, 0

    while True:
        yield (i, j)
        if cube_sum(i + 1, j) < cube_sum(i, j + 1):
            i += 1
        else:
            j += 1


