import sample2


def sampleFunction(a: int, b: int) -> int:
    # HELLO! THIS IS A COMMENT!
    if a > b:
        return a
    else:
        return b


sample2.sampleFunctionTwo(1, sampleFunction(1, 2))
