from threading_cleanup import Thread, Semaphore, watcher
import os, sys
from PIL import Image 
from typing import List, Dict, Callable, Tuple
import numpy as np

MAX_THREADS : int = 500

TRANSFORMATION = dict()

TRANSFORMATION["switch-r-b"] = \
    lambda rgb : (rgb[2], rgb[1], rgb[0])

TRANSFORMATION["invert"] = \
    lambda rgb : (100, 100, 100)

TRANSFORMATION["identity"] = \
    lambda rgb : rgb

CAN_DRAW_SECTION : Semaphore = None

def imagePointMap(image : np.ndarray, transformation):
    
    newImage : np.ndarray = np.apply_along_axis( \
        lambda list : transformation((list[0], list[1], list[2])), 2, image)

    return newImage

def transformThread(pixelMap : Image, transformation, \
                    rowLimits : Tuple[int, int], \
                    colLimits : Tuple[int, int]) -> None:
    
    global CAN_DRAW_SECTION
    box : Tuple[int, int, int, int] = \
        (rowLimits[0], colLimits[0], rowLimits[1], colLimits[1])

    CAN_DRAW_SECTION.wait() 
    imageSection : np.ndarray = np.array(pixelMap.crop(box))
    CAN_DRAW_SECTION.signal()

    transformedArray : np.ndarray = imagePointMap(imageSection, transformation)

    transformedImage : Image = Image.fromarray(transformedArray, mode = "RGB")

    CAN_DRAW_SECTION.wait() 
    pixelMap.paste(transformedImage, box)
    CAN_DRAW_SECTION.signal()

def getSubsectionLimits(numRows : int, numCols : int, \
                        width : int, height : int) \
                            -> List[Tuple[Tuple[int, int], Tuple[int, int]]]:
    
    subsectionWidth  = width  // numCols 
    subsectionHeight = height // numRows
    colRanges = [
        (c, min(c + subsectionHeight, height)) \
            for c in range(0, height, subsectionHeight)
    ]
    rowRanges = [
        (r, min(r + subsectionWidth, width)) \
            for r in range(0, width, subsectionWidth)
    ]

    limits = [
        (rowRange, colRange) \
            for rowRange in rowRanges for colRange in colRanges
    ]

    return limits 


def main(args): 
    global CAN_DRAW_SECTION, TRANSFORMATION
    if (len(args) == 1):
        print("Not enough arguments")
        exit(1)
    args = args[1:]
    CAN_DRAW_SECTION = Semaphore(1)
    
    if (len(args) != 3 and len(args) != 5):
        print("Invalid Input")
        exit(1)

    imageFile    : str = args[0]
    outputFile   : str = args[1]
    functionName : str = args[2]

    numRows : int = 1 if len(args) == 3 else int(args[3])
    numCols : int = 1 if len(args) == 3 else int(args[4])


    with Image.open(imageFile) as image:

        width, height = image.size

        subsectionRanges : list = \
            getSubsectionLimits(numRows, numCols, width, height)


        if (functionName not in TRANSFORMATION):
            print("Invalid function")
            exit(1)
        transformation = TRANSFORMATION[functionName]
        

        threads : list = [] 
        for rowRange, colRange in subsectionRanges:
            thread : Thread = Thread(transformThread, \
                                    image, transformation, rowRange, colRange)
            threads.append(thread)
        
        for thread in threads:
            thread.join()

        image.save(outputFile)

        

    exit(0)

if __name__ == '__main__':
    main(sys.argv)