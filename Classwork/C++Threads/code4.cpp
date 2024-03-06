/*
 * Demonstrates condition variables, unique locks, lambdas.
 *
 * Unique locks are an RAII pattern wrapper for mutexes:  Mutex is
 * acquired on initialization of the unique lock and released when the
 * destructor for the unique lock runs (even in the presence of
 * exceptions). 
 *
 * Lambda syntax:  [...captured vars...](...args...) { ...body...}
 * Using [&] means to capture all free variables in body by reference.
 */
#include <iostream>
#include <thread>
#include <condition_variable>
#include <mutex>
#include <chrono>
#include <queue>
using namespace std;


condition_variable cond_var;
mutex m;

int main()
{
        int value = 100;
        bool notified = false;
        
        thread reporter {[&]() {
                                 unique_lock<mutex> lock {m};
                                 while (not notified) {
                                         cond_var.wait(lock);
                                 }
                                 cout << "The value is " << value << endl;
                         }
                        };

        thread assigner {[&]() {
                                 value = 20;
                                 notified = true;
                                 cond_var.notify_one();
                         }
                        };
        reporter.join();
        assigner.join();

        return 0;
}

