#!/usr/bin/env python
#
# Example of a race condition on a shared variable fixed with a lock.
# In this case, there is a chance a mutator will fail.  You can optionally
# call the program with the denominator in the probability of failure:
#
# race_on_counter_random_bug.py 10
#
# means there is a 1 in 10 chance of a bug during each mutation operation. 
# The default is 10,000
#
# 
#
# Mark A. Sheldon
# Tufts University
# Fall 2014
#

import threading
import random
import sys

N_MUTATORS      =     10
N_ITERATIONS    = 100000

popular_counter =     0
counter_lock    = threading.Lock()

def mutator():
    global popular_counter
    for i in range(N_ITERATIONS):
        counter_lock.acquire()
        popular_counter += 1
        if bug_happened():
            raise RuntimeError('Bug, ouch!')
        counter_lock.release()

def main():
    threads = [threading.Thread(target = mutator, args = [])
               for i in range(N_MUTATORS)]
    for thread in threads:
        thread.start()
    for thread in threads:
        thread.join()
    print("Counter's final value:  ", popular_counter)


# I can't think of a good name for the following variable.
# The idea is that a mutator may fail with a probability of 
# 1 in N, and this is N.
FAILURE_DENOMINATOR = 100000   # 100,000
UNLUCKY_NUMBER      = random.randint(1, FAILURE_DENOMINATOR)

def bug_happened():
    return random.randint(1, FAILURE_DENOMINATOR) == UNLUCKY_NUMBER 
    

if __name__ == '__main__':
    if len(sys.argv) == 2:
        FAILURE_DENOMINATOR = int(sys.argv[1])
    main()
