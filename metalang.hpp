#ifndef _METALANG_H_
#define _METALANG_H_


struct __dummy_arg {};


template <int _val>
struct meta_integer {
        static const int value = _val;
        static const unsigned int __type_id = 0xffffffff;
};


template<class T, class U>
struct __is_same {
        typedef meta_integer<0> value;
};
 
template<class T>
struct __is_same<T, T> {
        typedef meta_integer<1> value;
};


template <typename a, typename b>
struct __meta_add {
        typedef meta_integer<a::value + b::value> value;
};

template <typename a, typename b>
struct __meta_sub {
        typedef meta_integer<a::value - b::value> value;
};

template <typename a, typename b>
struct __meta_mul {
        typedef meta_integer<a::value * b::value> value;
};

template <typename a, typename b>
struct __meta_div {
        typedef meta_integer<a::value / b::value> value;
};

template <typename a, typename b>
struct __meta_mod {
        typedef meta_integer<a::value % b::value> value;
};

template <typename a, typename b>
struct __meta_lt {
        typedef meta_integer<(a::value < b::value)> value;
};

template <typename a, typename b>
struct __meta_gt {
        typedef meta_integer<(a::value > b::value)> value;
};

template <typename a, typename b>
struct __meta_le {
        typedef meta_integer<(a::value <= b::value)> value;
};

template <typename a, typename b>
struct __meta_ge {
        typedef meta_integer<(a::value >= b::value)> value;
};

template <typename a, typename b>
struct __meta_eq {
        typedef meta_integer<(a::value == b::value)> value;
};

template <typename a, typename b>
struct __meta_ne {
        typedef meta_integer<(a::value != b::value)> value;
};

template <typename a, typename b>
struct __meta_and {
        typedef meta_integer<(a::value && b::value)> value;
};

template <typename a, typename b>
struct __meta_or {
        typedef meta_integer<(a::value || b::value)> value;
};

template <typename a>
struct __meta_not {
        typedef meta_integer<!a::value> value;
};

template <typename a>
struct __meta_neg {
        typedef meta_integer<- a::value> value;
};




#endif /* _METALANG_H_ */
