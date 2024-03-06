from WorkQueue import WorkQueue
from threading import Lock 
from ThreadingCleanup import Thread 
from sys import stdin

class FileWordFrequency:
    def __init__(self, outputFile, maxThreads : int = 10):
        self.outputFile         = outputFile 
        self.maxConsumers       = maxThreads - 1
        self.totalFrequency     = dict() 
        self.totalFrequencyLock = Lock()
        self.fileQueue          = WorkQueue()
        self.running            = True

        self.run()
        
    def run(self):
        threads = [Thread(self.readFileThread) for _ in \
                                               range(self.maxConsumers)]
        for fileName in stdin:
            if fileName.strip():
                self.fileQueue.produce(fileName.strip())

        self.running = False 

        for thread in threads:
            thread.join()
        
        self.fileQueue.finish()

    def readFileThread(self):
        while self.running or not self.fileQueue.isEmpty():
            fileName = self.fileQueue.consume()
            if not fileName:
                return
            with open(fileName) as file:
                lines         = file.readlines()
                fileFrequency = getWordFrequency(lines)

                FileWordFrequency.printFileHistogram(fileName, fileFrequency)

                self.addToFrequency(fileFrequency)
                self.printTotalHistogram()

    def addToFrequency(self, frequencies : dict):
        with self.totalFrequencyLock:
            for word, frequency in frequencies.items():
                if word in self.totalFrequency:
                    self.totalFrequency[word] += frequency 
                else:
                    self.totalFrequency[word] = frequency

    def printTotalHistogram(self):
        with self.totalFrequencyLock:
            sortedWords = sorted(list(self.totalFrequency.keys()))
            for word in sortedWords:
                frequency = self.totalFrequency[word]
                printLine = "{} {}\n" \
                    .format(word, frequency)
                self.outputFile.write(printLine)

    def printFileHistogram(fileName : str, histogram : dict):
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