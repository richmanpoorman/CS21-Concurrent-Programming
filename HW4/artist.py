'''
    Name: Matthew Wong 
    Date: 22 February 2024
    Use : Represents the artist drawing on the canvas
'''
from PIL import Image

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
            self.placePixel(newPosition, canvas, imageSize)
    
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