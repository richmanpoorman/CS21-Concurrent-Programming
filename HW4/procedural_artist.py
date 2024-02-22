from threading_cleanup import Thread, Semaphore
import os, sys
from PIL import Image
from typing import List, Dict, Callable, Tuple
import numpy as np
from math import ceil
from random import randrange
from threading import Lock

from artist import Artist

def makeArtists(numArtists : int, canvasSize : tuple) -> list:
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
    artist.runForSteps(numSteps, canvas, canvasSize, lock)

def runArtists(numArtists : int, numSteps : int, \
               canvasSize : tuple = (512, 512), \
               outputFileName : str = "canvas.jpg") -> None:
    
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