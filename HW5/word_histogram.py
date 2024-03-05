import sys 

from FileWordFrequency import getWordFrequency

def main(args):
    print(getWordFrequency(args[1]))

if __name__ == '__main__':
    main(sys.argv)