import numpy as np

def solve(input_file: str, debug: bool = False):
    with open(input_file) as fh:
        input = fh.readlines()

    input = [eval(i)[0] for i in input]

    inp = np.array(input)

    o_mask = inp[:,0] >= 0
    co_mask = inp[:,0] >= 0
    for i in range(inp.shape[1]):
        if sum(o_mask) > 1:
            gamma = (inp[o_mask,:].sum(axis=0) >= 0.5 * sum(o_mask)).astype(int)
            if debug:
                print('-----')
                print(inp[o_mask,:])
                print('g', gamma)
            o_mask &= inp[:,i] == gamma[i]
        if sum(co_mask) > 1:
            epsilon = (inp[co_mask,:].sum(axis=0) < 0.5 * sum(co_mask)).astype(int)
            if debug:
                print('-----')
                print(inp[co_mask,:])
                print('e', epsilon)
            co_mask &= inp[:,i] == epsilon[i]

    if debug:
        print(sum(o_mask), sum(co_mask))
        print(inp[co_mask, :])
        print(inp[o_mask, :])gs
        

    binaries = 2**(np.arange(inp.shape[1])[::-1])
    if debug:
        print(binaries)

    ans = (inp[co_mask, :] * binaries).sum() * (inp[o_mask, :] * binaries).sum()

    return ans

print(solve("test_input.txt", True))

print(solve("input.txt", False))