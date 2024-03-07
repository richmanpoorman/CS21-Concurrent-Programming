from collections import deque
from threading import Lock, Condition

class WorkQueue:
    
    def __init__(self):
        '''
            Name    : __init__
            Param   : (None)
            Purpose : Initializes a work queue
            Return  : (None)
        '''
        self.queue     = deque() 
        self.queueLock = Lock()
        self.hasItems  = Condition(self.queueLock)
        self.isDone    = False
    
    def finish(self):
        '''
            Name    : finish
            Param   : (None)
            Purpose : Signals all of the threads and set the state to finished
            Return  : (None)
        '''
        with self.queueLock:
            self.isDone = True 
            self.hasItems.notifyAll()

    def isEmpty(self):
        '''
            Name    : isEmpty
            Param   : (None)
            Purpose : Returns true if the queue is empty, false otherwise
            Return  : (bool) Whether the queue is empty or not
        '''
        with self.queueLock:
            return not self.queue

    def produce(self, item):
        '''
            Name    : produce
            Param   : (Any) item := The item to add to the work queue
            Purpose : Adds the given to the work queue, and alerts a consumer
                      that the queue has been updated
            Return  : (None)
        '''
        with self.queueLock:
            self.queue.append(item)
            self.hasItems.notify()
        
    def consume(self): 
        '''
            Name    : consume
            Param   : (None)
            Purpose : Returns an item from the queue if possible, or waits 
                      until it can acquire something from the queue
            Return  : (Any) The item from the queue, or None if the work
                      queue is finished
        '''
        with self.queueLock:
            while not self.queue:
                if self.isDone:
                    return None
                self.hasItems.wait()
            return self.queue.popleft()
        
    