#include <iostream>
#include <vector>
#include <thread>

//        vvvvvv----- Atomics support attomic types:  mutex built-in
#include <atomic>
using namespace std;

// How many threads we are including
#define THREAD_COUNT 200
#define INCREMENT 1

atomic<int> accum {0};  // Look Ma, no mutex!

void add(int x)
{
        accum += x;
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
