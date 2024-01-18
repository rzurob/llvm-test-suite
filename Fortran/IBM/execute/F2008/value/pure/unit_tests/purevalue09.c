/*****************************************************************************
  ===========================================================================
  XL Fortran Test Case                                  IBM INTERNAL USE ONLY
  ===========================================================================

  TEST CASE NAME  : F2008/value/pure/unit_tests/func/purevalue09.c
  TEST CASE TITLE : F2008: VALUE attr allowed for dummy args of PURE proc
  PROGRAMMER      : Gaby Baghdadi
  DATE            : 2010-12-01
  ORIGIN          : XL Fortran Compiler Development, IBM Torolab
  DRIVER STANZA   : xlc

  DESCRIPTION
  - used in conjunction with purevalue09.f
*****************************************************************************/


#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <curses.h>

int foo (
    signed char, 
    short, 
    int, 
    long, 
    long long, 
    size_t, 
    intptr_t, 
    intmax_t, 
    int8_t, 
    int16_t, 
    int32_t, 
    int64_t, 
    int_least8_t, 
    int_least16_t, 
    int_least32_t, 
    int_least64_t, 
    int_fast8_t, 
    int_fast16_t, 
    int_fast32_t, 
    int_fast64_t, 
    float, 
    double, 
    long double, 
    float _Complex, 
    double _Complex, 
    long double _Complex, 
    _Bool, 
    char );

void sub (
    int *,
    short, 
    int, 
    long, 
    long long, 
    size_t, 
    intptr_t, 
    intmax_t, 
    int8_t, 
    int16_t, 
    int32_t, 
    int64_t, 
    int_least8_t, 
    int_least16_t, 
    int_least32_t, 
    int_least64_t, 
    int_fast8_t, 
    int_fast16_t, 
    int_fast32_t, 
    int_fast64_t, 
    float, 
    double, 
    long double, 
    float _Complex, 
    double _Complex, 
    long double _Complex, 
    _Bool, 
    char );

int main()
{
    signed char          ac_signed_char;
    short                ac_short;
    int                  ac_int;
    long                 ac_long;
    long long            ac_long_long;
    size_t               ac_size_t;
    intptr_t             ac_intptr_t;
    intmax_t             ac_intmax_t;
    int8_t               ac_int8_t;
    int16_t              ac_int16_t;
    int32_t              ac_int32_t;
    int64_t              ac_int64_t;
    int_least8_t         ac_int_least8_t;
    int_least16_t        ac_int_least16_t;
    int_least32_t        ac_int_least32_t;
    int_least64_t        ac_int_least64_t;
    int_fast8_t          ac_int_fast8_t;
    int_fast16_t         ac_int_fast16_t;
    int_fast32_t         ac_int_fast32_t;
    int_fast64_t         ac_int_fast64_t;
    float                ac_float, aco_float;
    double               ac_double, aco_double;
    long double          ac_long_double, aco_long_double;
    float _Complex       ac_float_Complex, aco_float_Complex; 
    double _Complex      ac_double_Complex, aco_double_Complex;
    long double _Complex ac_long_double_complex, aco_long_double_complex;
    _Bool                ac__Bool;
    char                 ac_char;

    int res;

    ac_signed_char   = 'a'; 
    ac_short         = 0x1234;
    ac_int           = 0x567890ab;

#ifdef __64BIT__
    ac_long          = 0xcdef012345678901;
    ac_size_t        = 0x234567890abcdef0;
    ac_intptr_t      = 0x1234567890abcdef;
#else
    ac_long          = 0xcdef0123;
    ac_size_t        = 0x4567890a;
    ac_intptr_t      = 0xbcdef012;
#endif

    ac_long_long     = 0x01234567890abcde;
    ac_intmax_t      = 0xf01234567890abcd;
    ac_int8_t        = 0x6f;
    ac_int16_t       = 0x0123;
    ac_int32_t       = 0x45678901;
    ac_int64_t       = 0x2345678901234567;
    ac_int_least8_t  = 0x59;
    ac_int_least16_t = 0x7bcd;
    ac_int_least32_t = 0x12345678;
    ac_int_least64_t = 0x90abcdef01234567;
    ac_int_fast8_t   = 0x34;
    ac_int_fast16_t  = 0x789a;
    ac_int_fast32_t  = 0x890abcde;
    ac_int_fast64_t  = 0x4567890123456789;

    ac_float         = aco_float = 1.2/3.4;
    ac_double        = aco_double = 5.6/7.8;

    ac_long_double         = aco_long_double = 9.1L/11.12L;
    ac_float_Complex       = aco_float_Complex = __cmplxf(13.14,15.16);
    ac_double_Complex      = aco_double_Complex = __cmplx(17.18L,19.20L);
    ac_long_double_complex = aco_long_double_complex =  __cmplxl(21.22L,23.24L);

    ac__Bool = TRUE; 
    ac_char = 'c';

    res = foo(
        ac_signed_char, 
        ac_short, 
        ac_int, 
        ac_long, 
        ac_long_long, 
        ac_size_t, 
        ac_intptr_t, 
        ac_intmax_t, 
        ac_int8_t, 
        ac_int16_t, 
        ac_int32_t, 
        ac_int64_t, 
        ac_int_least8_t, 
        ac_int_least16_t, 
        ac_int_least32_t, 
        ac_int_least64_t, 
        ac_int_fast8_t, 
        ac_int_fast16_t, 
        ac_int_fast32_t, 
        ac_int_fast64_t, 
        ac_float, 
        ac_double, 
        ac_long_double, 
        ac_float_Complex, 
        ac_double_Complex, 
        ac_long_double_complex, 
        ac__Bool, 
        ac_char);

    if (ac_signed_char != 'a') exit(1); 
    if (ac_short != 0x1234) exit(2);
    if (ac_int != 0x567890ab) exit(3);

#ifdef __64BIT__
    if (ac_long != 0xcdef012345678901) exit(4);
    if (ac_size_t != 0x234567890abcdef0) exit(5);
    if (ac_intptr_t != 0x1234567890abcdef) exit(6);
#else
    if (ac_long != 0xcdef0123) exit(7);
    if (ac_size_t != 0x4567890a) exit(8);
    if (ac_intptr_t != 0xbcdef012) exit(9);
#endif

    if (ac_long_long != 0x01234567890abcde) exit(10);
    if (ac_intmax_t != 0xf01234567890abcd) exit(11);
    if (ac_int8_t != 0x6f) {printf("%d\n",ac_int8_t); exit(12);}
    if (ac_int16_t != 0x0123) exit(13);
    if (ac_int32_t != 0x45678901) exit(14);
    if (ac_int64_t != 0x2345678901234567) exit(15);
    if (ac_int_least8_t != 0x59) exit(16);
    if (ac_int_least16_t != 0x7bcd) exit(17);
    if (ac_int_least32_t != 0x12345678) exit(18);
    if (ac_int_least64_t != 0x90abcdef01234567) exit(19);
    if (ac_int_fast8_t != 0x34) exit(20);
    if (ac_int_fast16_t != 0x789a) exit(21);
    if (ac_int_fast32_t != 0x890abcde) exit(22);
    if (ac_int_fast64_t != 0x4567890123456789) exit(23);
    if (ac_float != aco_float) exit(24);
    if (ac_double != aco_double) exit(25);

    if (ac_long_double != aco_long_double) exit(26);
    if (ac_float_Complex != aco_float_Complex) exit(27);
    if (ac_double_Complex != aco_double_Complex) exit(28);
    if (ac_long_double_complex != aco_long_double_complex) exit(29);

    if (ac__Bool != TRUE) exit(30);
    if (ac_char != 'c') exit(31);

    if (res != 12345) exit(-1);

    sub(
        &res, 
        ac_short, 
        ac_int, 
        ac_long, 
        ac_long_long, 
        ac_size_t, 
        ac_intptr_t, 
        ac_intmax_t, 
        ac_int8_t, 
        ac_int16_t, 
        ac_int32_t, 
        ac_int64_t, 
        ac_int_least8_t, 
        ac_int_least16_t, 
        ac_int_least32_t, 
        ac_int_least64_t, 
        ac_int_fast8_t, 
        ac_int_fast16_t, 
        ac_int_fast32_t, 
        ac_int_fast64_t, 
        ac_float, 
        ac_double, 
        ac_long_double, 
        ac_float_Complex, 
        ac_double_Complex, 
        ac_long_double_complex, 
        ac__Bool, 
        ac_char);


    if (ac_signed_char != 'a') exit(101); 
    if (ac_short != 0x1234) exit(102);
    if (ac_int != 0x567890ab) exit(103);

#ifdef __64BIT__
    if (ac_long != 0xcdef012345678901) exit(104);
    if (ac_size_t != 0x234567890abcdef0) exit(105);
    if (ac_intptr_t != 0x1234567890abcdef) exit(106);
#else
    if (ac_long != 0xcdef0123) exit(107);
    if (ac_size_t != 0x4567890a) exit(108);
    if (ac_intptr_t != 0xbcdef012) exit(109);
#endif

    if (ac_long_long != 0x01234567890abcde) exit(110);
    if (ac_intmax_t != 0xf01234567890abcd) exit(111);
    if (ac_int8_t != 0x6f) {printf("%d\n",ac_int8_t); exit(112);}
    if (ac_int16_t != 0x0123) exit(113);
    if (ac_int32_t != 0x45678901) exit(114);
    if (ac_int64_t != 0x2345678901234567) exit(115);
    if (ac_int_least8_t != 0x59) exit(116);
    if (ac_int_least16_t != 0x7bcd) exit(117);
    if (ac_int_least32_t != 0x12345678) exit(118);
    if (ac_int_least64_t != 0x90abcdef01234567) exit(119);
    if (ac_int_fast8_t != 0x34) exit(120);
    if (ac_int_fast16_t != 0x789a) exit(121);
    if (ac_int_fast32_t != 0x890abcde) exit(122);
    if (ac_int_fast64_t != 0x4567890123456789) exit(123);
    if (ac_float != aco_float) exit(124);
    if (ac_double != aco_double) exit(125);

    if (ac_long_double != aco_long_double) exit(126);
    if (ac_float_Complex != aco_float_Complex) exit(127);
    if (ac_double_Complex != aco_double_Complex) exit(128);
    if (ac_long_double_complex != aco_long_double_complex) exit(129);

    if (ac__Bool != TRUE) exit(130);
    if (ac_char != 'c') exit(131);

    if (res != 12345) exit(-2);
    exit(0);
}

