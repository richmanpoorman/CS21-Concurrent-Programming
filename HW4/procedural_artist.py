'''
    Name: Matthew Wong 
    Date: 22 February 2024
    Use : Runs program which simulates artists on a canvas
          
    Run : 
        python3 prodcural_artist.py -M [number of artists] -S [number of steps]
'''

from threading_cleanup import Thread, Semaphore
import os, sys
from PIL import Image
from typing import List, Dict, Callable, Tuple
import numpy as np
from math import ceil
from random import randrange
from threading import Lock

class Artist:
    '''
        Name    : Artist
        Purpose : A class representing each artist choosing to draw a pixel
    '''
    # The color white 
    WHITE = (255, 255, 255) 
    # Constants for the directions
    LEFT, UP, RIGHT, DOWN = 0, 1, 2, 3
    # Tuples representing going one step in that direction
    IN_DIRECTION = [(-1, 0), (0, 1), (1, 0), (0, -1)]


    def __init__(self, startPos : tuple, color : tuple):
        '''
            Name       : __init__
            Purpose    : Initializes a new instance of the artist
            Parameters : (int, int)      startPos := The starting position
                                                     of the artist
                         (int, int, int) color    := The color to draw with
                                                
        '''
        self.x, self.y = startPos 
        self.color     = color 
        self.owned     = [startPos]

    def placePixel(self, position : tuple, \
                   canvas, imageSize : tuple): 
        
        '''
            Name       : placePixel
            Purpose    : Given a new pixel position to go to, places the pixel
                         of the artist's color if the space is free, or 
                         randomly chooses from the owned pixels to go to
                         if the pixel is already taken or goes out of bounds
            Parameters : (int, int)    position  := The position on the canvas
                                                    that the artist tries to 
                                                    put the pixel
                         (PixelAccess) canvas    := The canvas to draw to
                         (int, int)    imageSize := The size of the canvas
            Return     : (None)
        '''
        x, y = position
        width, height = imageSize

        # Picks a new pixel if the new position is invalid
        if (x < 0 or y < 0 or x >= width or y >= height or \
            canvas[x, y] != Artist.WHITE):
            self.x, self.y = self.owned[randrange(0, len(self.owned))]
            return

        # Places the pixel if unowned
        canvas[position] = self.color
        self.owned.append(position) 
        self.x, self.y = position 

    def pickNextPosition(self, canvas, imageSize : tuple, lock : Lock):
        '''
            Name       : pickNextPosition
            Purpose    : Concurrently picks a direction from the current pixel,
                         and tries to place the pixel there, using the lock
                         to make sure placing is atomic
            Parameters : (PixelAccess) canvas    := The canvas to draw to
                         (int, int)    imageSize := The size of the canvas 
                         (Lock)        lock      := The lock of the canvas
                                                    which allows for concurrent
                                                    updating of the image
            Return     : (None)
        '''
        direction = randrange(0, 4)
        dX, dY = Artist.IN_DIRECTION[direction]
        newPosition = (self.x + dX, self.y + dY)
        with lock:
            # print("+", self.color, "Picks up the lock")
            self.placePixel(newPosition, canvas, imageSize)
            # print("-", self.color, "Drops the lock")
    
    def runForSteps(self, numSteps : int, \
                    canvas, imageSize : tuple, lock : Lock):
        '''
            Name       : runForSteps
            Purpose    : Tries to paint for numSteps number of steps
            Parameters : (int)         numSteps  := The number of steps
                         (PixelAccess) canvas    := The canvas to draw to 
                         (int, int)    imageSize := The size of the canvas
                         (Lock)        lock      := The lock of the canvas
                                                    which allows for concurrent
                                                    updating of the image
            Return     : (None)
        '''
        for step in range(numSteps):
            self.pickNextPosition(canvas, imageSize, lock)

    
def makeArtists(numArtists : int, canvasSize : tuple) -> list:
    '''
        Name       : makeArtist
        Purpose    : Initializes all of the artists on the canvas
        Parameters : (int)      numArtists := The number of artists to create
                     (int, int) canvasSize := The size of the canvas
        Return     : (List[Artist]) The list of initialized artists
    '''
    width, height = canvasSize

    artists : list = list()
    seenPosition : set = set() 
    seenColor : set = set()
    for id in range(numArtists):
        position = (randrange(0, width), randrange(0, height))
        while position in seenPosition:
            position = (randrange(0, width), randrange(0, height))
        
        color = (randrange(0, 256), randrange(0, 256), randrange(0, 256))
        while color in seenColor:
            color = (randrange(0, 256), randrange(0, 256), randrange(0, 256))
        
        artist : Artist = Artist(position, color) 
        artists.append(artist)

    return artists

def artistThread(artist : Artist, numSteps : int, \
                 canvas, canvasSize : tuple, lock : Lock):
    '''
        Name       : artistThread
        Purpose    : A thread which runs the artist for numSteps
        Parameters : (Artist)      artist     := The artist of the thread
                     (int)         numSteps   := The number of steps to run for
                     (PixelAccess) canvas     := The canvas to draw to
                     (int, int)    canvasSize := The size of the canvas
                     (Lock)        lock       := The lock which controls the 
                                                 canvas access
        Return     : (None)
    '''
    artist.runForSteps(numSteps, canvas, canvasSize, lock)

def runArtists(numArtists : int, numSteps : int, \
               canvasSize : tuple = (512, 512), \
               outputFileName : str = "canvas.jpg") -> None:
    '''
        Name       : runArtists
        Purpose    : Creates a new image, and simulates artists on the canvas
        Parameters : (int)      numArtists     := The number of artists
                                                  to simulate 
                     (int)      numSteps       := The number of steps to run 
                                                  each artist
                     (int, int) canvasSize     := [OPTIONAL] The size of the 
                                                  canvas, default (512, 512)
                     (str)      outputFileName := [OPTIONAL] The file name 
                                                  to save the image to, 
                                                  default "canvas.jpg"
        Return     : (None)
    '''
    
    image : Image = Image.new("RGB", canvasSize, Artist.WHITE)
    canvas = image.load() 
    
    artists : list = makeArtists(numArtists, canvasSize)
    
    lock : Lock = Lock()

    threads : list = list() 
    for artist in artists: 
        thread : Thread = Thread(artistThread, \
                                 artist, numSteps, canvas, canvasSize, lock)
        threads.append(thread)

    for thread in threads:
        thread.join() 

    image.save(outputFileName)

def main(args):
    '''
        Name       : main
        Purpose    : Given the arguments, simulates artists on a canvas
        Parameters : (list) args := The number of threads and number of steps
        Return     : (None)
    '''
    if (len(args) != 5):
        print("Wrong arguments given; expecting \
              'procedural_artist.py -M number-of-threads -S number-of-steps'")
        exit(1)
    
    args = args[1:]

    numArtists, numSteps = 0, 0
    if (args[0] == '-M' and args[2] == '-S'):
        numArtists = int(args[1])
        numSteps   = int(args[3])
    elif (args[0] == '-S' and args[2] == '-M'):
        numSteps   = int(args[1])
        numArtists = int(args[3])
    else:
        print("No thread count given")
        exit(1)

    runArtists(numArtists, numSteps)

    exit(0)
    

    
if __name__ == '__main__':
    main(sys.argv)