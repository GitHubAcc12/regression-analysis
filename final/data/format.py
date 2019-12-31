


if __name__=='__main__':
    lines = []
    with open('Bluegill.txt', 'r') as inputfile:
        lines = inputfile.readlines()
        print(lines)

    with open('Bluegill.txt', 'w') as outputfile:
        outputfile.write(lines[0])
        for i in range(1, len(lines)):
            outputfile.write(f'"{i}"\t'+lines[i])