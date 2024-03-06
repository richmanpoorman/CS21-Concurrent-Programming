#include <iostream>
#include <vector>
#include <thread>
#include <mutex>

//        vvvvvv------- chrono supports timing
#include <chrono>

using namespace std;

using ns       = chrono::nanoseconds;
using get_time = chrono::steady_clock;

// How many threads we are including
#define THREAD_COUNT 200
#define INCREMENT 1

int   accum = 0;      // BAD!  Shared global variable
mutex accum_mutex;    // <------- a mutex to protect accumulator

void add(int x)
{
        accum_mutex.lock();
        accum += x;
        accum_mutex.unlock();
}

int main()
{
        vector<thread> ths;

        auto start = get_time::now();
        
        for (int i = 0; i <= THREAD_COUNT; i++) {
                ths.push_back(thread {&add, INCREMENT});
        }

        for (auto &th : ths) {
                th.join();
        }

        cout << "accum = " << accum << endl;

        auto end  = get_time::now();
        auto diff = end - start;
        cout << "Elapsed time:  " 
             << diff.count()
                //<< chrono::duration_cast<ns>(diff).count()
             << " ns" 
             << endl;

        return 0;
}
