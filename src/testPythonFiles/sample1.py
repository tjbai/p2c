def sampleFunction(a: int, b: int) -> int:
    c = 1
    c = a + b

    while True:
        a -= 1
    return a


def callFunction():
    if True:
        return False
    else:
        return True

    return sampleFunction(10, 5)


if True:
    sampleFunction(10, 5)


callFunction()
