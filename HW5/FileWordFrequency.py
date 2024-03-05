


def getWordFrequency(fileName : str):
    '''
        Name    : getWordFrequency
        Param   : (str) fileName := The file to parse through
        Purpose : Gets the frequency of the tokens in the file 
        Return  : (dict) A dictionary with (word, frequency) key-pair values
    '''
    with open(fileName, 'r') as file:
        frequencyDict = dict() 
        for line in file: 
            rawWords = line.split(' ')
            for rawWord in rawWords:
                word = rawWord.lower().strip()
                if not word: 
                    continue 

                if word in frequencyDict:
                    frequencyDict[word] += 1 
                else:
                    frequencyDict[word] = 1
        
        return frequencyDict