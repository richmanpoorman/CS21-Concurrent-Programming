from threading_cleanup import Thread, Semaphore, watcher
import os, sys
from PIL import Image 
from typing import List, Dict, Callable, Tuple


MAX_THREADS : int = 500

TRANSFORMATION = \
    Dict[str, Callable[[Tuple[int, int, int]], Tuple[int, int, int]]]


CAN_DRAW_SECTION : Semaphore = None

def transformThread(pixelMap : Image, transformFunc : function, \
                    rowLimits : Tuple[int, int, int], \
                    colLimits : Tuple[int, int, int]) -> None:
    
    global CAN_DRAW_SECTION
    box : Tuple[int, int, int, int] = \
        (rowLimits[0], colLimits[0], rowLimits[1], colLimits[1])

    imageSection : Image = pixelMap \
        .crop(box) \
        .point(transformFunc) 
    
    CAN_DRAW_SECTION.wait() 
    pixelMap.paste(imageSection, box)
    CAN_DRAW_SECTION.signal()

def getSubsectionLimits(numRows : int, numCols : int, \
                        width : int, height : int) \
                            -> List[Tuple[Tuple[int, int], Tuple[int, int]]]:
    pass 

def main(): 
    global CAN_DRAW_SECTION

    args : list = sys.argv[1:]
    CAN_DRAW_SECTION = Semaphore(1)

    if (len(args) != 3 or len(args) != 5):
        print("Invalid Input")
        exit(1)

    imageFile    : str = args[0]
    printFile    : str = args[1]
    functionName : str = args[2]

    numRows : int = 1 if len(args) == 3 else args[3]
    numCols : int = 1 if len(args) == 3 else args[4]

    





if __name__ == '__main__':
    main(sys.argv)