#ifndef __MY_SEQUENCE_HPP__
#define __MY_SEQUENCE_HPP__

#include <iostream> // TODO: cleanup
#include <iterator> // std::forward_iterator_tag
#include <limits>   // std::numeric_limits
#include <utility>  // std::declval

// #include <cfenv> FE_OVERFLOW | FE_UNDERFLOW
//  std::feclearexcept(FE_OVERFLOW);
//  max double + min double
//    if(std::fetestexcept(FE_OVERFLOW)) {

namespace my {
    template<typename T> T min_step();
    template<> inline constexpr int min_step<int>() { return 1; }

    template<typename T> T min_val() {
        return std::numeric_limits<T>::min();
    }
    template<typename T> T max_val() {
        return std::numeric_limits<T>::max();
    }

    template<typename Gen>
    struct GeneratorTraits {
        using value_type = typename Gen::value_type;
        using difference_type = typename Gen::difference_type;
    };

    template<bool Const, typename Gen>
    class GeneratorIterator {
    public:
        using T                 = typename GeneratorTraits<Gen>::value_type;
        using difference_type   = typename GeneratorTraits<Gen>::difference_type;
        using value_type        = std::conditional_t<Const, const T , T >;
        using pointer           = std::conditional_t<Const, const T*, T*>;
        using reference         = std::conditional_t<Const, const T&, T&>;
        // or bidirectional iterator if prev() is defined
        using iterator_category = std::forward_iterator_tag;

        GeneratorIterator()                            = delete;
        GeneratorIterator(const GeneratorIterator& gi) = default;
        GeneratorIterator(GeneratorIterator&& gi)      = default;
        GeneratorIterator(const Gen& gen) : _gen{gen} {
            //std::cout << "GeneratorIterator(const Gen&)(" << gen << ")\n";
        }

        GeneratorIterator& operator=(const GeneratorIterator&) = delete;
        GeneratorIterator& operator=(GeneratorIterator&&)      = delete;

        GeneratorIterator& operator++() {
            _gen.next();
            return *this;
        }

        GeneratorIterator operator++(int) {
            auto tmp(*this);
            _gen.next();
            return tmp;
        }

        // Any reason to make it a reference?
        value_type operator*() const {
            return _gen.value();
        }

        bool operator==(const GeneratorIterator& rhs) const {
            return _gen == rhs._gen;
        }

        bool operator!=(const GeneratorIterator& rhs) const {
            return _gen != rhs._gen;
        }

    private:
        Gen _gen;
    };

    template<typename Gen>
    class Sequence {
    public:
        using value_type     = typename GeneratorTraits<Gen>::value_type;
        using iterator       = GeneratorIterator<false, Gen>;
        using const_iterator = GeneratorIterator<true , Gen>;

        Sequence(const Gen& gen) : _gen{gen} {
            //std::cout << "Sequence(const Gen&)(" << _gen << ")\n";
        }

        iterator begin() { return iterator{_gen}; }
        iterator end()   { return iterator{Gen{}}; }

        const_iterator cbegin() const { return const_iterator{_gen}; }
        const_iterator cend()   const { return const_iterator{Gen{}}; }

    private:
        Gen _gen;
    };

    /*
    class Filter {
    public:
        Filter(Sequence&);
    };
    */

} // namespace my

#endif // __MY_SEQUENCE_HPP__
