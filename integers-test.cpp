//
// $ sudo apt install libgtest-dev
// $ g++ -Wall integers-test.cpp `pkg-config --cflags --libs gtest` && ./a.out
//

#include <iostream>
#include <numeric>
#include <gtest/gtest.h>
#include "integers.hpp"

using namespace std;

TEST(IntegersSuite, FromOnly) {
    auto g = my::integers(1);
    auto p = g.begin();
    auto e = g.end();
    EXPECT_TRUE(p != e);
    EXPECT_FALSE(p == e);

    EXPECT_EQ(1, *p);
    EXPECT_EQ(2, *(++p));
    EXPECT_EQ(3, *(++p));
    EXPECT_EQ(4, *(++p));
    EXPECT_EQ(5, *(++p));
    ++p;

    EXPECT_TRUE(p != e);
    EXPECT_FALSE(p == e);
}

TEST(IntegersSuite, FromTo) {
    auto g = my::integers(1, 5);
    auto p = g.begin();
    auto e = g.end();
    EXPECT_TRUE(p != e);
    EXPECT_FALSE(p == e);

    EXPECT_EQ(1, *p);
    EXPECT_EQ(2, *(++p));
    EXPECT_EQ(3, *(++p));
    EXPECT_EQ(4, *(++p));
    ++p;

    EXPECT_TRUE(p == e);
    EXPECT_FALSE(p != e);
}

TEST(IntegersSuite, FromToNeg) {
    auto g = my::integers(-3, 1);
    auto p = g.begin();
    auto e = g.end();
    EXPECT_TRUE(p != e);
    EXPECT_FALSE(p == e);

    EXPECT_EQ(-3, *p);
    EXPECT_EQ(-2, *(++p));
    EXPECT_EQ(-1, *(++p));
    EXPECT_EQ( 0, *(++p));
    ++p;

    EXPECT_TRUE(p == e);
    EXPECT_FALSE(p != e);
}

TEST(IntegersSuite, FromToStep) {
    auto i = my::integers(0, 5, 2);
    auto p = i.begin();
    auto e = i.end();
    EXPECT_TRUE(p != e);
    EXPECT_FALSE(p == e);

    EXPECT_EQ(0, *p);
    EXPECT_EQ(2, *(++p));
    EXPECT_EQ(4, *(++p));
    ++p;

    EXPECT_TRUE(p == e);
    EXPECT_FALSE(p != e);
}

TEST(IntegersSuite, FromToStepRev) {
    auto g = my::integers(5, 0, -1);
    auto p = g.begin();
    auto e = g.end();
    EXPECT_TRUE(p != e);
    EXPECT_FALSE(p == e);

    EXPECT_EQ(5, *p);
    EXPECT_EQ(4, *(++p));
    EXPECT_EQ(3, *(++p));
    EXPECT_EQ(2, *(++p));
    EXPECT_EQ(1, *(++p));
    ++p;

    EXPECT_TRUE(p == e);
    EXPECT_FALSE(p != e);
}

TEST(IntegersSuite, Duplicate) {
    auto g = my::integers(1, 2, 0);
    auto p = g.begin();
    auto e = g.end();
    EXPECT_TRUE(p != e);
    EXPECT_FALSE(p == e);

    EXPECT_EQ(1, *p);
    EXPECT_EQ(1, *(++p));
    EXPECT_EQ(1, *(++p));
    ++p;

    EXPECT_TRUE(p != e);
    EXPECT_FALSE(p == e);
}

TEST(IntegersSuite, PreIncPostIncIterators) {
    auto g = my::integers(1, 5);
    auto p = g.begin();
    auto e = g.end();
    EXPECT_TRUE(p != e);
    EXPECT_FALSE(p == e);

    EXPECT_EQ(1, *p);
    EXPECT_EQ(2, *(++p));
    EXPECT_EQ(2, *(p++));
    EXPECT_EQ(3, *p);
    p++;
    ++p;

    EXPECT_TRUE(p == e);
    EXPECT_FALSE(p != e);
}

TEST(IntegersSuite, ForLoop) {
    auto g = my::integers(1, 10+1);
    int sum = 0;
    for (auto it = g.begin(); it != g.end(); ++it) {
        sum += *it;
    }
    EXPECT_EQ(55, sum);
}

TEST(IntegersSuite, ForRange) {
    int sum = 0;
    for (auto x : my::integers(1, 10+1)) {
        sum += x;
    }
    EXPECT_EQ(55, sum);
}

TEST(IntegersSuite, Accumulate) {
    auto g = my::integers(1, 10+1);
    int sum = std::accumulate(g.begin(), g.end(), 0);
    EXPECT_EQ(55, sum);
}

TEST(IntegersSuite, PosIntegersTestFrom) {
    auto g = my::pos_integers();
    auto p = g.begin();
    auto e = g.end();
    EXPECT_EQ(1, *p);
    EXPECT_EQ(2, *(++p));
    EXPECT_EQ(3, *(++p));
    EXPECT_EQ(4, *(++p));
    EXPECT_EQ(5, *(++p));
    ++p;
    EXPECT_TRUE(p != e);
}

TEST(IntegersSuite, NonNegIntegersTestFrom) {
    auto g = my::non_neg_integers();
    auto p = g.begin();
    auto e = g.end();
    EXPECT_EQ(0, *p);
    EXPECT_EQ(1, *(++p));
    EXPECT_EQ(2, *(++p));
    EXPECT_EQ(3, *(++p));
    EXPECT_EQ(4, *(++p));
    ++p;
    EXPECT_TRUE(p != e);
}

int main(int argc, char* argv[]) {
    ::testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}
