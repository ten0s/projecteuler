#ifndef __MY_INTEGERS_HPP__
#define __MY_INTEGERS_HPP__

#include <cassert>
#include <ostream>
#include <functional>
#include <optional>
#include "sequence.hpp"

namespace my {
    template<typename T, typename GenFn>
    class NumbersGenerator {
    public:
        using value_type = T;
        using difference_type = T;

        NumbersGenerator()
            : _curr{std::nullopt}, _end{}, _step{}, _next_fn{nullptr} {}

        NumbersGenerator(T init, T end, T step, const GenFn& next_fn)
            : _curr{init}, _end{end}, _step{step}, _next_fn{next_fn} {
                assert((init <= end && step >= 0) || (init > end && step < 0));

                _threshold = next_fn(end, -step);
                if (init <= end && step >= 0) {
                    _cmp_fn = std::less<T>();
                } else {
                    _cmp_fn = std::greater<T>();
                }
            }

        NumbersGenerator(const NumbersGenerator& n) = default;
        NumbersGenerator(NumbersGenerator&&) = default;

        NumbersGenerator& operator=(const NumbersGenerator&) = delete;
        NumbersGenerator& operator=(NumbersGenerator&&) = delete;

        void next() noexcept {
            if (_curr.has_value()) {
                if (_cmp_fn(_curr.value(), _threshold)) {
                    _curr = _next_fn(_curr.value(), _step);
                } else {
                    _curr.reset();
                }
            }
        }

        // Throws a std::bad_optional_access exception
        T value() const {
            return _curr.value();
        }

        bool is_done() const noexcept {
            return !_curr.has_value();
        }

        bool operator==(const NumbersGenerator& rhs) const {
            return _curr == rhs._curr;
        }

        bool operator!=(const NumbersGenerator& rhs) const {
            return _curr != rhs._curr;
        }

        friend std::ostream& operator<<(std::ostream& os, const NumbersGenerator& ng) {
            os << "#NumbersGenerator{" << ng._curr.value_or(-1) << "}";
            return os;
        }

    private:
        std::optional<T> _curr;
        T _end;
        T _step;
        std::function<T(T, T)> _next_fn;
        T _threshold;
        std::function<bool(T, T)> _cmp_fn;
    };

    /* TODO
    template<typename T, typename GenFn>
    NumbersGenerator<T, GenFn> numbers_generator(T init, const GenFn& next_fn) {
        return NumbersGenerator<T, GenFn>{init, next_fn};
    }
    */

    struct IntegersGenerator : NumbersGenerator<int, std::function<int(int, int)>> {
        IntegersGenerator()
            : NumbersGenerator<int, std::function<int(int, int)>>{} {}
        IntegersGenerator(int init)
            : IntegersGenerator{init, max_val<int>()} {}
        IntegersGenerator(int init, int end)
            : IntegersGenerator{init, end, min_step<int>()} {}
        IntegersGenerator(int init, int end, int step)
            : NumbersGenerator<int, std::function<int(int, int)>>{init, end, step, std::plus<int>()} {}
    };

    using IntegersSequence = Sequence<IntegersGenerator>;

    IntegersSequence integers(int init) {
        return IntegersSequence{IntegersGenerator{init}};
    }

    IntegersSequence integers(int init, int end) {
        return IntegersSequence{IntegersGenerator{init, end}};
    }

    IntegersSequence integers(int init, int end, int step) {
        return IntegersSequence{IntegersGenerator{init, end, step}};
    }

    IntegersSequence pos_integers() {
        return IntegersSequence{IntegersGenerator{1}};
    }

    IntegersSequence non_neg_integers() {
        return IntegersSequence{IntegersGenerator{0}};
    }

} // namespace my

#endif // __MY_INTEGERS_HPP__
