import sys 

from FileWordFrequency import FileWordFrequency

def main(args):
    '''
        Name    : main
        Param   : (list) args := The arguments called with on execution
        Purpose : Runs the word_histogram program, and opens files
        Return  : (None)
    '''
    if len(args) != 3 and len(args) != 5:
        print("Wrong number of arguments")
        exit(1)

    outputFileName = None 
    numThreads     = 10
    if args[1] == '-m':
        numThreads = int(args[2])
    elif args[1] == '-o':
        outputFileName = args[2]
    else:
        print("Expected an -o or -m tag")
        exit(1)

    if len(args) == 5:
        if args[1] == '-o' and args[3] == '-m':
            numThreads = int(args[4])
        elif args[1] == '-m' and args[3] == '-o':
            outputFileName = args[4]
        else:
            print("Expected a second -o or -m tag")
            exit(1)

    if outputFileName == None:
        print("Expected File Name")
        exit(1)
    
    if numThreads < 2:
        print("Expected at least 2 threads")
        exit(1)
    
    with open(outputFileName, 'w') as outputFile:
        FileWordFrequency(outputFile, numThreads)



if __name__ == '__main__':
    main(sys.argv)