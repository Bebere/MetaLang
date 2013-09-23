#include <iostream>
#include "metalang.hpp"


struct __lambda_0{
        template <typename f, typename _0>
        struct apply {
                struct __lambda_1{
                        template <typename h, typename _1>
                        struct apply {
                                struct __lambda_2{
                                        template <typename x, typename _2>
                                        struct apply {
                                                typedef typename f::template apply<typename h::template apply<h, __dummy_arg>::value, __dummy_arg>::value::template apply<x, __dummy_arg>::value value;
                                        };
                                };
                                typedef __lambda_2 value;
                        };
                };
                struct __lambda_2{
                        template <typename h, typename _2>
                        struct apply {
                                struct __lambda_3{
                                        template <typename x, typename _3>
                                        struct apply {
                                                typedef typename f::template apply<typename h::template apply<h, __dummy_arg>::value, __dummy_arg>::value::template apply<x, __dummy_arg>::value value;
                                        };
                                };
                                typedef __lambda_3 value;
                        };
                };
                typedef typename __lambda_1::template apply<__lambda_2, __dummy_arg>::value value;
        };
};
struct __lambda_1{
        template <typename q, typename _1>
        struct apply {
                struct __lambda_2{
                        template <typename x, typename _2>
                        struct apply {
                                template <typename _t4, typename _4>
                                struct __branch_4{
                                        typedef meta_integer<1> value;
                                };
                                template <typename _4>
                                struct __branch_4<meta_integer<0>, _4> {
                                        typedef typename __meta_mul<x, typename q::template apply<typename __meta_sub<x, meta_integer<1> >::value, __dummy_arg>::value >::value value;
                                };
                                typedef typename __branch_4< typename __meta_eq<x, meta_integer<0> >::value , __dummy_arg >::value value;
                        };
                };
                typedef __lambda_2 value;
        };
};
typedef __lambda_0::apply<__lambda_1, __dummy_arg>::value factorial;

typedef factorial::apply<meta_integer<0>, __dummy_arg>::value fac0;

typedef factorial::apply<meta_integer<1>, __dummy_arg>::value fac1;

typedef factorial::apply<meta_integer<2>, __dummy_arg>::value fac2;

typedef factorial::apply<meta_integer<3>, __dummy_arg>::value fac3;

typedef factorial::apply<meta_integer<4>, __dummy_arg>::value fac4;

typedef factorial::apply<meta_integer<5>, __dummy_arg>::value fac5;


// Passing metalang values as template parameters would cause a compile error, which would
// dump the template parameter in deduced form
template <int _>
struct dump {};

int main(int argc, char *argv[])
{
        dump<fac0> _0;
        dump<fac1> _1;
        dump<fac2> _2;
        dump<fac3> _3;
        dump<fac4> _4;
        dump<fac5> _5;
        
        return 0;
}
