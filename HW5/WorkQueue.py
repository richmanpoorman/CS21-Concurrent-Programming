from collections import deque
from threading import Lock, Condition

class WorkQueue:
    
    def __init__(self):
        self.queue     = deque() 
        self.queueLock = Lock()
        self.hasItems  = Condition(self.queueLock)
        self.isDone    = False
    
    def finish(self):
        with self.queueLock:
            self.isDone = True 
            self.hasItems.notifyAll()

    def isEmpty(self):
        with self.queueLock:
            return not self.queue

    def produce(self, item):
        with self.queueLock:
            self.queue.append(item)
            self.hasItems.notify()
        
    def consume(self): 
        with self.queueLock:
            while not self.queue:
                if self.isDone:
                    return None
                self.hasItems.wait()
            return self.queue.popleft()
        
    