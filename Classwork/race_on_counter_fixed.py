#!/usr/bin/env python
#
# Example of a race condition on a shared variable fixed with a lock.
#
# Mark A. Sheldon
# Tufts University
# Fall 2014
#

import threading

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
    for thread in threads:
        thread.start()
    for thread in threads:
        thread.join()
    print("Counter's final value:  ", popular_counter)


if __name__ == '__main__':
    main()
