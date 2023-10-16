def the_nat_pairs_cubesum():
    def cubesum_gen():
        i, j = 0, 0
        while True:
            cubesum_i = i ** 3
            cubesum_j = j ** 3
            if cubesum_i + cubesum_j < cubesum_gen():
                yield (i, j)
                i += 1
            else:
                j += 1

    return cubesum_gen()
