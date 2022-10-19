#ifndef __MY_FIBONACCI_HPP__
#define __MY_FIBONACCI_HPP__

#include <ostream>
#include "sequence.hpp"

namespace my {
    template<typename T>
    class FibonacciGenerator {
    public:
        using value_type = T;
        using difference_type = T;

        FibonacciGenerator() : _a{}, _b{}, _end{}, _is_done{true} {}
        FibonacciGenerator(T a, T b, T end) : _a{a}, _b{b}, _end{end}, _is_done{false} {}
        FibonacciGenerator(const FibonacciGenerator& n) = default;
        FibonacciGenerator(FibonacciGenerator&&) = default;

        FibonacciGenerator& operator=(const FibonacciGenerator&) = delete;
        FibonacciGenerator& operator=(FibonacciGenerator&&) = delete;

        void next() noexcept {
            if (!_is_done) {
                T next = _b + _a;
                if (next < _end) {
                    _a = _b;
                    _b = next;
                } else {
                    _is_done = true;
                }
            }
        }

        T value() const noexcept {
            return _b;
        }

        bool is_done() const noexcept {
            return _is_done;
        }

        bool operator==(const FibonacciGenerator& rhs) const {
            return is_done() == rhs.is_done() || value() == rhs.value();
        }

        bool operator!=(const FibonacciGenerator& rhs) const {
            return !(*this == rhs);
        }

        friend std::ostream& operator<<(std::ostream& os, const FibonacciGenerator& g) {
            os << "#FibonacciGenerator{";
            os << "value=" << g.value() << ", ";
            os << "is_done=" << g.is_done();
            os << "}";
            return os;
        }

    private:
        T _a;
        T _b;
        T _end;
        bool _is_done;
    };

    template<typename T>
    using FibonacciSequence = Sequence<FibonacciGenerator<T>>;

    template<typename T>
    FibonacciSequence<T> fibonacci(T a, T b, T max) {
        return FibonacciSequence<T>{FibonacciGenerator<T>{a, b, max}};
    }

    template<typename T>
    FibonacciSequence<T> fibonacci(T a, T b) {
        return fibonacci<T>(a, b, max_val<T>());
    }

    FibonacciSequence<int> fibonacci01() {
        return fibonacci(0, 1);
    }

    FibonacciSequence<int> fibonacci11() {
        return fibonacci(1, 1);
    }

} // namespace my

#endif // __MY_FIBONACCI_HPP__
