f = open('input.txt', 'r')

target = 2020

if f.mode == 'r':
    contents = f.read()
    arr = sorted([int(ele) for ele in contents.split('\n') if int(ele) <= target])

    i = 0
    j = len(arr) - 1
    while i < j:
        if arr[i] + arr[j] < target:
            i += 1
        elif arr[i] + arr[j] > target:
            j -= 1
        else:
            print({'i': arr[i], 'j': arr[j], 'mult': arr[i] * arr[j]})
            break
