'''
    Name: Matthew Wong 
    Date: 21 February 2024
    Use : Runs program which transforms a given image
          
    Run : 
        python3 transform_image.py [image] [output file] [transform]
        or
        python3 transform_image.py [image] [output file] [transform] \
            [number of rows] [number of columns]
'''

from threading_cleanup import Thread, Semaphore
import os, sys
from PIL import Image 
from typing import List, Tuple
import numpy as np
from math import ceil

''' GLOBAL VARIABLE SPECIFICATIONS '''
MAX_THREADS : int = 500

''' TRANSFORMATION OPERATIONS '''
TRANSFORMATION = dict()
# Swaps the red and blue components of each pixel
TRANSFORMATION["switch-r-b"] = \
    lambda rgb : (rgb[2], rgb[1], rgb[0])

# Performs a color inversion on the image
TRANSFORMATION["invert"] = \
    lambda rgb : (255 - rgb[0], 255 - rgb[1], 255 - rgb[2])

# Reduces the brightness by %50
TRANSFORMATION["dim"] = \
    lambda rgb : (rgb[0] // 2, rgb[1] // 2, rgb[2] // 2)

# Does nothing to the image
TRANSFORMATION["identity"] = \
    lambda rgb : (rgb[0], rgb[1], rgb[2])

''' RUN PROGRAM '''
CAN_DRAW_SECTION : Semaphore = None

def imagePointMap(image : np.ndarray, transformation):
    '''
        Name       : imagePointMap
        Purpose    : Performs the transformation on each pixel of the image
        Parameters : (np.ndarray) image := The image data as an ndarray 
                     (function)   transformation := A function which takes in
                                                    a tuple reprsenting a 
                                                    pixel and returns a tuple
                                                    representing a pixel
        Return     : (np.ndarray) A uint8 ndarray which represents the 
                                  new image after mapping
    '''
    newImage : np.ndarray = np.apply_along_axis( \
        lambda list : transformation((list[0], list[1], list[2])), 2, image)
    return np.uint8(newImage)

def transformThread(pixelMap : Image, transformation, \
                    rowLimits : Tuple[int, int], \
                    colLimits : Tuple[int, int]) -> None:
    '''
        Name       : transformThread
        Purpose    : A thread that can draw the subsection of the image given
                     specified by the row and column limits
        Parameters : (Image)      pixelMap       := The image to alter
                     (function)   transformation := The operation to alter by
                     ((int, int)) rowLimits      := The row range to edit 
                     ((int, int)) colLimits      := The column range to edit
        Return     : (None)
    '''
    global CAN_DRAW_SECTION
    box : Tuple[int, int, int, int] = \
        (rowLimits[0], colLimits[0], rowLimits[1], colLimits[1])

    # Get the portion of the image to work on
    CAN_DRAW_SECTION.wait() 
    imageSection : np.ndarray = np.array(pixelMap.crop(box))
    CAN_DRAW_SECTION.signal()

    # Perform the transformation
    transformedArray : np.ndarray = \
        imagePointMap(imageSection, transformation)

    transformedImage : Image = Image.fromarray(transformedArray, mode = "RGB")

    # Put the new altered portion back on the image
    CAN_DRAW_SECTION.wait() 
    pixelMap.paste(transformedImage, box)
    CAN_DRAW_SECTION.signal()

def getSubsectionLimits(numRows : int, numCols : int, \
                        width : int, height : int) \
                            -> List[Tuple[Tuple[int, int], Tuple[int, int]]]:
    
    '''
        Name       : getSubsectionLimits
        Purpose    : Finds the set of pixel ranges for the threads to run on
        Parameters : (int) numRows := The number of rows to break up into 
                     (int) numCols := The number of columns to break up into
                     (int) width   := The width of the image in pixels
                     (int) height  := The height of the image in pixels
        Return     : (List[(Tuple, Tuple)] A list containing tuples, which 
                                           contain the row range and column 
                                           range of the subsection
    '''

    subsectionWidth  = ceil(width  / numCols)
    subsectionHeight = ceil(height / numRows)

    # Get the column and row ranges
    colRanges = [
        (c, min(c + subsectionHeight, height)) \
            for c in range(0, height, subsectionHeight)
    ]
    rowRanges = [
        (r, min(r + subsectionWidth, width)) \
            for r in range(0, width, subsectionWidth)
    ]

    # Combine them to form a list of all of the parts of the grid
    limits = [
        (rowRange, colRange) \
            for rowRange in rowRanges for colRange in colRanges
    ]

    return limits 

def transformImage(image : Image, transformation, \
                   numRows : int, numCols : int):
    
    '''
        Name       : transformImage
        Purpose    : Transforms the image in parallel by breaking it up 
                     into numRows x numCols sections, and concurrently 
                     performing the transformation on each pixel of the image
        Parameters : (Image)    image          := The image to transform 
                     (function) transformation := The operation to transform
                                                  the image with
                     (int)      numRows        := The number of rows to break
                                                  the image up into to 
                                                  process concurrently
                     (int)      numCols        := The number of columns to 
                                                  break the image up into to
                                                  process concurrently
        Return     : (None)
    '''

    width, height = image.size

    # Cap the number of rows/cols, so there are no 0-pixel regions
    numRows = min(numRows, width)
    numCols = min(numCols, height)

    # Get the division coordinates of the threads
    subsectionRanges : list = \
        getSubsectionLimits(numRows, numCols, width, height)
    
    # Make and run threads concurrently on each block
    threads : list = [] 
    for rowRange, colRange in subsectionRanges:
        thread : Thread = Thread(transformThread, \
                                image, transformation, rowRange, colRange)
        threads.append(thread)
    
    # Wait for all sections to be done
    for thread in threads:
        thread.join()

def main(args): 
    '''
        Name       : main
        Purpose    : Runs the program which takes an image and performs
                     a transformation on the image 
        Parameters : (List[str]) args := Image file, output file, 
                                         transformation, and (optionally) 
                                         number of rows and columns to 
                                         subdivide the image into
        Return     : (None)
    '''
    global CAN_DRAW_SECTION, TRANSFORMATION

    # Checks if arguments were given
    if (len(args) == 1):
        print("No arguments were given: expected 3 or 5 arguments")
        exit(1)
    args = args[1:]
    CAN_DRAW_SECTION = Semaphore(1)
    
    # Checks if correct amount of arguments given
    if (len(args) != 3 and len(args) != 5):
        print("Wrong number of arguments given: got", len(args), \
              "but expected 3 or 5 arguments")
        exit(1)

    imageFile    : str = args[0]
    outputFile   : str = args[1]
    functionName : str = args[2]

    numRows : int = 1 if len(args) == 3 else int(args[3])
    numCols : int = 1 if len(args) == 3 else int(args[4])

    # Checks if not max amount of threads
    if (numRows * numCols > MAX_THREADS):
        print("Too many threads to run: can run at most", MAX_THREADS, \
              "threads")
        exit(1)
    
    # Checks if function is valid
    if (functionName not in TRANSFORMATION):
            print("Invalid function given; expected one of:", \
                  list(TRANSFORMATION.keys()))
            exit(1)
    
    # Performs the transformation
    with Image.open(imageFile) as image:
        
        transformation = TRANSFORMATION[functionName]
        
        transformImage(image, transformation, numRows, numCols)

        image.save(outputFile)
        
    exit(0)

if __name__ == '__main__':
    main(sys.argv)