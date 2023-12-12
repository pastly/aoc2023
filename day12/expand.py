import sys

def main(infd, outfd):
    for line in infd:
        line = line.strip()
        words = line.split(' ')
        first = '?'.join([words[0]] * 5)
        second = ','.join([words[1]] * 5)
        outfd.write(f'{first} {second}\n')

if __name__ == '__main__':
    main(sys.stdin, sys.stdout)
