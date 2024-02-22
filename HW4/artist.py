
from PIL import Image

from random import randrange
from threading import Lock


class Artist:
    # The color white 
    WHITE = (255, 255, 255) 
    # Constants for the directions
    LEFT, UP, RIGHT, DOWN = 0, 1, 2, 3
    # Tuples representing going one step in that direction
    IN_DIRECTION = [(-1, 0), (0, 1), (1, 0), (0, -1)]

    def __init__(self, startPos : tuple, color : tuple):
        self.x, self.y = startPos 
        self.color     = color 
        self.owned     = []

    def placePixel(self, position : tuple, \
                   canvas, imageSize : tuple): 
        x, y = position
        width, height = imageSize

        # Picks a new pixel if the new position is invalid
        if (x < 0 or y < 0 or x >= width or y >= height or \
            canvas[x, y] != Artist.WHITE):
            if (len(self.owned) == 0):
                return
            self.x, self.y = self.owned[randrange(0, len(self.owned))]
            return

        # Places the pixel if unowned
        canvas[position] = self.color
        self.owned.append(position) 
        self.x, self.y = position 

    def pickNextPosition(self, canvas, imageSize : tuple, lock : Lock):
        direction = randrange(0, 4)
        dX, dY = Artist.IN_DIRECTION[direction]
        newPosition = (self.x + dX, self.y + dY)
        with lock:
            self.placePixel(newPosition, canvas, imageSize)
    
    def runForSteps(self, numSteps : int, \
                    canvas, imageSize : tuple, lock : Lock):
        for step in range(numSteps):
            self.pickNextPosition(canvas, imageSize, lock)