#include <iostream>

//        vvvvvv------ Programming in the future!  From 1980s?
#include <future>
#include <chrono>

using namespace std;

int square(int x)
{
        return x * x;
}

int main()
{
        future<int> a = async(&square, 10);

        int v = a.get();

        cout << "The thread returned " << v << endl;

        return 0;
}
