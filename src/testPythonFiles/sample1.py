def sampleFunction(a: int, b: int) -> int:
    while a != 0 and b != 0:
        if a - b > 0:
            a = a - b
        else:
            return a
