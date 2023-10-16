def string_fset_at(cs, i0, c0):
    return ''.join([cs[i] if i != i0 else c0 for i in range(len(cs))])

alphabet = ''.join([chr(ord('a') + i) for i in range(26)])

def list_of_buddies(word):
    n0 = len(word)
    
    def work(callback):
        for i0 in range(n0):
            c0 = word[i0]
            for c1 in alphabet:
                if c1 != c0:
                    callback(string_fset_at(word, i0, c1))
    
    result = []
    work(result.append)
    return result

