#include <iostream>
//        vvvvvv------ Include the thread library
#include <thread>

using namespace std;

// A simple test function
void test(int x)
{
        cout << "Hello from our thread!  It prints " << x << "!" << endl;
}

int main()
{
        // Create a new thread and pass in with one parameter
        thread th {&test, 100};


	// Continue executing the main thread
        cout << "Hello from the main thread!" << endl;

        th.join();  // Wait for thread to finish

        return 0;
}
