def getIncrementTo100(max: int) -> int:
    i = 0
    while i < max:
        i += 1
        print(i)
    return i


def getUserInput() -> int:
    output = 0
    output = input("Enter a number: ")
    return output
