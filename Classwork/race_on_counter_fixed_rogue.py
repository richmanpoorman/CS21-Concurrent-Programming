#!/usr/bin/env python
#
# Example of a race condition on a shared variable fixed with a lock.
#
# Mark A. Sheldon
# Tufts University
# Fall 2014
#

import threading
import random

N_MUTATORS      =     10
N_ITERATIONS    = 100000

popular_counter =     0
counter_lock    = threading.Lock()

def mutator():
    global popular_counter
    for i in range(N_ITERATIONS):
        counter_lock.acquire()
        popular_counter += 1
        counter_lock.release()

def main():
    threads = [threading.Thread(target = mutator, args = [])
               for i in range(N_MUTATORS)]
    threads.append(threading.Thread(target = rogue_mutator, args = []))
    for thread in threads:
        thread.start()
    for thread in threads:
        thread.join()
    print("Counter's final value:  ", popular_counter)


def rogue_mutator():
    global popular_counter
    taunts = [line for line in open('taunts.text', 'r')]
    for i in range(N_ITERATIONS):
        if i % 1000 == 0:
            print(random.choice(taunts))
        popular_counter += 1

if __name__ == '__main__':
    main()
