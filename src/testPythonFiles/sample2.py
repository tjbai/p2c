def getIncrementTo100(max: int) -> int:
    i = 0
    while i < max:
        if i % 3 == 0:
            print("\ndivisible by 3", i)
        elif i % 2 == 0:
            print("\ndivisible by 2", i)
        else:
            print("\nnot divisible by 3 or 2", i)
        i += 1
    return i
