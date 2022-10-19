//
// $ sudo apt install libgtest-dev
// $ g++ -Wall fibonacci-test.cpp `pkg-config --cflags --libs gtest` && ./a.out
//

#include <iostream>
#include <numeric>
#include <gtest/gtest.h>
#include "fibonacci.hpp"

using namespace std;

TEST(FibonacciSuite, Fib01) {
    auto g = my::fibonacci01();
    auto p = g.begin();
    auto e = g.end();
    EXPECT_TRUE(p != e);
    EXPECT_FALSE(p == e);

    EXPECT_EQ(1, *p);
    EXPECT_EQ(1, *(++p));
    EXPECT_EQ(2, *(++p));
    EXPECT_EQ(3, *(++p));
    EXPECT_EQ(5, *(++p));
    ++p;

    EXPECT_TRUE(p != e);
    EXPECT_FALSE(p == e);
}

TEST(FibonacciSuite, Fib11) {
    auto g = my::fibonacci11();
    auto p = g.begin();
    auto e = g.end();
    EXPECT_TRUE(p != e);
    EXPECT_FALSE(p == e);

    EXPECT_EQ(1, *p);
    EXPECT_EQ(2, *(++p));
    EXPECT_EQ(3, *(++p));
    EXPECT_EQ(5, *(++p));
    EXPECT_EQ(8, *(++p));
    ++p;

    EXPECT_TRUE(p != e);
    EXPECT_FALSE(p == e);
}

TEST(FibonacciSuite, Fib01Max) {
    auto g = my::fibonacci(0, 1, 10);
    auto p = g.begin();
    auto e = g.end();
    EXPECT_TRUE(p != e);
    EXPECT_FALSE(p == e);

    EXPECT_EQ(1, *p);
    EXPECT_EQ(1, *(++p));
    EXPECT_EQ(2, *(++p));
    EXPECT_EQ(3, *(++p));
    EXPECT_EQ(5, *(++p));
    EXPECT_EQ(8, *(++p));
    ++p;

    EXPECT_TRUE(p == e);
    EXPECT_FALSE(p != e);
}

TEST(FibonacciSuite, Fib11Max) {
    auto g = my::fibonacci(1, 1, 10);
    auto p = g.begin();
    auto e = g.end();
    EXPECT_TRUE(p != e);
    EXPECT_FALSE(p == e);

    EXPECT_EQ(1, *p);
    EXPECT_EQ(2, *(++p));
    EXPECT_EQ(3, *(++p));
    EXPECT_EQ(5, *(++p));
    EXPECT_EQ(8, *(++p));
    ++p;

    EXPECT_TRUE(p == e);
    EXPECT_FALSE(p != e);
}

TEST(IntegersSuite, ForLoop) {
    auto g = my::fibonacci(1, 1, 10);
    int sum = 0;
    for (auto it = g.begin(); it != g.end(); ++it) {
        sum += *it;
    }
    EXPECT_EQ(19, sum);
}

TEST(IntegersSuite, ForRange) {
    int sum = 0;
    for (auto x : my::fibonacci(1, 1, 10)) {
        sum += x;
    }
    EXPECT_EQ(19, sum);
}

TEST(IntegersSuite, Accumulate) {
    auto g = my::fibonacci(1, 1, 10);
    int fact = std::accumulate(g.begin(), g.end(), 0);
    EXPECT_EQ(19, fact);
}

int main(int argc, char* argv[]) {
    ::testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}
