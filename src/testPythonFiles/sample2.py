def sampleFunction(a: int, b: int) -> int:
    while a > b:
        a -= 1
    return a


def callFunction():
    return sampleFunction(10, 5)


callFunction()
