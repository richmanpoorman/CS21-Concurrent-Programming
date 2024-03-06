#include <iostream>
#include <vector>
#include <thread>
//        vvvv------Include mutexes
#include <mutex>

using namespace std;

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

        for (int i = 0; i <= THREAD_COUNT; i++) {
                ths.push_back(thread {&add, INCREMENT});
        }

        for (auto &th : ths) {
                th.join();
        }

        cout << "accum = " << accum << endl;
        return 0;
}
