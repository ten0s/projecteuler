#include <numeric>
#include <iostream>
#include "fibonacci.hpp"

using namespace std;

int main() {
    const int MAX = 4'000'000;

    {
        int sum = 0;

        int f1 = 1, f2 = 1, tmp;
        while (f2 < MAX) {
            if (f2 % 2 == 0) {
                sum += f2;
            }
            tmp = f1;
            f1 = f2;
            f2 += tmp;
        }

        cout << sum << endl;
    }

    {
        int sum = 0;
        auto g = my::fibonacci(1, 1, MAX);
        for (auto it = g.begin(); it != g.end(); ++it) {
            if (*it % 2 == 0) {
                sum += *it;
            }
        }
        cout << sum << endl;
    }

    {
        int sum = 0;
        for (auto x : my::fibonacci(1, 1, MAX)) {
            if (x % 2 == 0) {
                sum += x;
            }
        }
        cout << sum << endl;
    }

    {
        int sum = 0;
        for (auto x : my::fibonacci11()) {
            if (x >= MAX) break;

            if (x % 2 == 0) {
                sum += x;
            }
        }
        cout << sum << endl;
    }

    {
        auto g = my::fibonacci(1, 1, MAX);
        int sum = accumulate(g.begin(), g.end(), 0, [](auto acc, auto x) {
            return (x % 2 == 0) ? x + acc : acc;
        });
        cout << sum << endl;
    }
}
