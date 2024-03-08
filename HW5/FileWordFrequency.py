from WorkQueue import WorkQueue
from threading import Lock 
from ThreadingCleanup import Thread 
from sys import stdin

class FileWordFrequency:
    def __init__(self, outputFile, maxThreads : int = 10):
        '''
            Name    : __init__
            Param   : (File) outputFile := The file to output the combined 
                                           histogram to 
                      (int)  maxThreads := The maximum total number of threads
            Purpose : Initializes and runs the 
            Return  : 
        '''
        self.outputFile         = outputFile 
        self.maxConsumers       = maxThreads - 1
        self.totalFrequency     = dict() 
        self.totalFrequencyLock = Lock()
        self.fileQueue          = WorkQueue()

        self.run(stdin)
        
    def run(self, inputFile):
        '''
            Name    : run
            Param   : (File) inputFile := The input stream with the file names
                                          to count the frequency of
            Purpose : Starts maxThreads - 1 consumer threads, and acts as a 
                      producer thread on the files by adding file names to 
                      the shared worker queue
            Return  : (None)
        '''
        threads = [Thread(self.readFileThread) for _ in \
                                               range(self.maxConsumers)]
        for fileName in inputFile:
            if fileName.strip():
                self.fileQueue.produce(fileName.strip())

        # Add the 'end' flag for the consumers to end themselves
        self.fileQueue.produce(None)

        for thread in threads:
            thread.join()
        
        self.printTotalHistogram()
        
        
    def readFileThread(self):
        '''
            Name    : readFileThread
            Param   : (None)
            Purpose : Consumer which takes file names off of the work queue, 
                      processes and prints the word frequency of the file, 
                      then adds the frequency to the shared combined frequency
                      histogram, and updates the output file
            Return  : (None)
        '''
        while True:
            fileName = self.fileQueue.consume()

            # End the consumer, and pass command to next consumer
            if not fileName:
                self.fileQueue.produce(None)
                return
            
            with open(fileName.strip()) as file:
                lines         = file.readlines()
                fileFrequency = getWordFrequency(lines)

                FileWordFrequency.printFileHistogram(fileName, fileFrequency)
                
                self.addToFrequency(fileFrequency)

    def addToFrequency(self, frequencies : dict):
        '''
            Name    : addToFrequency
            Param   : (dict) frequencies := The frequency of words to add to 
                                            the combined histogram
            Purpose : Adds frequencies of words to the combined histogram
            Return  : None
        '''
        with self.totalFrequencyLock:
            for word, frequency in frequencies.items():
                if word in self.totalFrequency:
                    self.totalFrequency[word] += frequency 
                else:
                    self.totalFrequency[word] = frequency

    def printTotalHistogram(self):
        '''
            Name    : printTotalHistogram
            Param   : (None)
            Purpose : Prints the combined histogram to the output file
            Return  : (None)
        '''
        with self.totalFrequencyLock:
            sortedWords = sorted(list(self.totalFrequency.keys()))
            for word in sortedWords:
                frequency = self.totalFrequency[word]
                printLine = "{} {}\n" \
                    .format(word, frequency)
                self.outputFile.write(printLine)

    def printFileHistogram(fileName : str, histogram : dict):
        '''
            Name    : printFileHistogram
            Param   : (str)  fileName  := The name of the file the histogram 
                                          is from
                      (dict) histogram := The word frequencies in the file
            Purpose : Prints out the frequency from the file after processing
            Return  : (None)
        '''
        sortedWords = sorted(list(histogram.keys()))
        for word in sortedWords:
            frequency = histogram[word]
            printLine = "{}:\t{} {}" \
                .format(fileName, word, frequency)
            print(printLine)


def getWordFrequency(lines : list):
    '''
        Name    : getWordFrequency
        Param   : (list) lines := The list of raw lines
        Purpose : Gets the frequency of the tokens in the file 
        Return  : (dict) A dictionary with (word, frequency) key-pair values
    '''
    frequencyDict = dict() 
    words = [word.strip().lower() for rawLine in lines \
                                  for word    in rawLine.split(' ') \
                                  if word.strip()]

    for word in words:
        if word in frequencyDict:
            frequencyDict[word] += 1
        else:
            frequencyDict[word] = 1
    
    return frequencyDict