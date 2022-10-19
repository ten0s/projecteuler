#include <algorithm>
#include <numeric>
#include <iostream>
#include <vector>
#include "integers.hpp"

using namespace std;

int main() {
    const int MAX = 1000;

    {
        int sum = 0;
        for (int i = 3; i < MAX; ++i) {
            if (i % 3 == 0 || i % 5 == 0) {
                sum += i;
            }
        }
        cout << sum << endl;
    }

    {
        int sum = 0;
        auto g = my::integers(1, MAX, 1);
        for (auto it = g.begin(); it != g.end(); ++it) {
            if (*it % 3 == 0 || *it % 5 == 0) {
                sum += *it;
            }
        }
        cout << sum << endl;
    }

    {
        int sum = 0;
        for (auto x : my::integers(1, MAX)) {
            if (x % 3 == 0 || x % 5 == 0) {
                sum += x;
            }
        }
        cout << sum << endl;
    }

    {
        int sum = 0;
        for (auto x : my::integers(1)) {
            if (x >= MAX) break;

            if (x % 3 == 0 || x % 5 == 0) {
                sum += x;
            }
        }
        cout << sum << endl;
    }

    {
        auto g = my::integers(1, MAX);
        int sum = accumulate(g.begin(), g.end(), 0, [](auto acc, auto x) {
            return (x % 3 == 0 || x % 5 == 0) ? x + acc : acc;
        });
        cout << sum << endl;
    }

    {
        auto g = my::integers(MAX - 1, 0, -1);
        int sum = accumulate(g.begin(), g.end(), 0, [](auto acc, auto x) {
            return (x % 3 == 0 || x % 5 == 0) ? x + acc : acc;
        });
        cout << sum << endl;
    }

    {
        vector<int> v;
        auto g = my::integers(1, MAX);
        copy_if(g.cbegin(), g.cend(), back_inserter(v), [](auto x) {
            return x % 3 == 0 || x % 5 == 0;
        });
        int sum = accumulate(v.cbegin(), v.cend(), 0);
        cout << sum << endl;
    }
}
