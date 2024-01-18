! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2010-10-16
!*  ORIGIN                     :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : - test all interoperable intrinsic types
!*                               - main Fortran program
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module mod

    use, intrinsic :: iso_c_binding

    integer(c_short)          i2
    integer(c_long)           i3

    integer(c_int32_t)        i9
    integer(c_int64_t)        i10

    integer(c_int_least32_t)  i13

    integer(c_int_fast16_t)   i16
    integer(c_int_fast32_t)   i17

    integer(c_intptr_t)       i20

end module

program main

    use mod
    implicit none

    integer(c_int)            i1
    integer(c_long_long)      i4

    integer(c_signed_char)    i5
    integer(c_size_t)         i6

    integer(c_int8_t)         i7
    integer(c_int16_t)        i8


    integer(c_int_least8_t)   i11
    integer(c_int_least16_t)  i12
    integer(c_int_least64_t)  i14

    integer(c_int_fast8_t)    i15
    integer(c_int_fast64_t)   i18

    integer(c_intmax_t)       i19


    real(c_float)             r1
    real(c_double)            r2
    real(c_long_double)       r3

    complex(c_float_complex)       c1
    complex(c_double_complex)      c2
    complex(c_long_double_complex) c3

    logical(c_bool)                ll
    character(c_char)              cc
    character                      cc2

    interface
        integer(C_SIZE_T) function get_size_i1(x) bind(c)
            use, intrinsic :: iso_c_binding
            integer(c_int)  x
        end function get_size_i1

        integer(C_SIZE_T) function get_size_i2(x) bind(c)
            use, intrinsic :: iso_c_binding
            integer(c_short) x
        end function get_size_i2

        integer(C_SIZE_T) function get_size_i3(x) bind(c)
            use, intrinsic :: iso_c_binding
            integer(c_long)  x
        end function get_size_i3

        integer(C_SIZE_T) function get_size_i4(x) bind(c)
            use, intrinsic :: iso_c_binding
            integer(c_long_long) x
        end function get_size_i4

        integer(C_SIZE_T) function get_size_i5(x) bind(c)
            use, intrinsic :: iso_c_binding
            integer(c_signed_char) x
        end function get_size_i5

        integer(C_SIZE_T) function get_size_i6(x) bind(c)
            use, intrinsic :: iso_c_binding
            integer(c_size_t) x
        end function get_size_i6

        integer(C_SIZE_T) function get_size_i7(x) bind(c)
            use, intrinsic :: iso_c_binding
            integer(c_int8_t) x
        end function get_size_i7

        integer(C_SIZE_T) function get_size_i8(x) bind(c)
            use, intrinsic :: iso_c_binding
            integer(c_int16_t) x
        end function get_size_i8

        integer(C_SIZE_T) function get_size_i9(x) bind(c)
            use, intrinsic :: iso_c_binding
            integer(c_int32_t) x
        end function get_size_i9

        integer(C_SIZE_T) function get_size_i10(x) bind(c)
            use, intrinsic :: iso_c_binding
            integer(c_int64_t) x
        end function get_size_i10

        integer(C_SIZE_T) function get_size_i11(x) bind(c)
            use, intrinsic :: iso_c_binding
            integer(c_int_least8_t) x
        end function get_size_i11

        integer(C_SIZE_T) function get_size_i12(x) bind(c)
            use, intrinsic :: iso_c_binding
            integer(c_int_least16_t) x
        end function get_size_i12

        integer(C_SIZE_T) function get_size_i13(x) bind(c)
            use, intrinsic :: iso_c_binding
            integer(c_int_least32_t) x
        end function get_size_i13

        integer(C_SIZE_T) function get_size_i14(x) bind(c)
            use, intrinsic :: iso_c_binding
            integer(c_int_least64_t) x
        end function get_size_i14

        integer(C_SIZE_T) function get_size_i15(x) bind(c)
            use, intrinsic :: iso_c_binding
            integer(c_int_fast8_t)  x
        end function get_size_i15

        integer(C_SIZE_T) function get_size_i16(x) bind(c)
            use, intrinsic :: iso_c_binding
            integer(c_int_fast16_t) x
        end function get_size_i16

        integer(C_SIZE_T) function get_size_i17(x) bind(c)
            use, intrinsic :: iso_c_binding
            integer(c_int_fast32_t) x
        end function get_size_i17

        integer(C_SIZE_T) function get_size_i18(x) bind(c)
            use, intrinsic :: iso_c_binding
            integer(c_int_fast64_t) x
        end function get_size_i18

        integer(C_SIZE_T) function get_size_i19(x) bind(c)
            use, intrinsic :: iso_c_binding
            integer(c_intmax_t)    x
        end function get_size_i19

        integer(C_SIZE_T) function get_size_i20(x) bind(c)
            use, intrinsic :: iso_c_binding
            integer(c_intptr_t)   x
        end function get_size_i20

        integer(C_SIZE_T) function get_size_r1(x) bind(c)
            use, intrinsic :: iso_c_binding
            real(c_float) x
        end function get_size_r1

        integer(C_SIZE_T) function get_size_r2(x) bind(c)
            use, intrinsic :: iso_c_binding
            real(c_double) x
        end function get_size_r2

        integer(C_SIZE_T) function get_size_r3(x) bind(c)
            use, intrinsic :: iso_c_binding
            real(c_long_double) x
        end function get_size_r3

        integer(C_SIZE_T) function get_size_c1(x) bind(c)
            use, intrinsic :: iso_c_binding
            complex(c_float_complex) x
        end function get_size_c1

        integer(C_SIZE_T) function get_size_c2(x) bind(c)
            use, intrinsic :: iso_c_binding
            complex(c_double_complex) x
        end function get_size_c2

        integer(C_SIZE_T) function get_size_c3(x) bind(c)
            use, intrinsic :: iso_c_binding
            complex(c_long_double_complex) x
        end function get_size_c3

        integer(C_SIZE_T) function get_size_ll(x) bind(c)
            use, intrinsic :: iso_c_binding
            logical(c_bool) x
        end function get_size_ll

        integer(C_SIZE_T) function get_size_cc(x) bind(c)
            use, intrinsic :: iso_c_binding
            character(c_char) x
        end function get_size_cc

    end interface

    if ( c_sizeof(i1) /= get_size_i1(i1) ) error stop 1
    if ( c_sizeof(i2) /= get_size_i2(i2) ) error stop 2
    if ( c_sizeof(i3) /= get_size_i3(i3) ) error stop 3
    if ( c_sizeof(i4) /= get_size_i4(i4) ) error stop 4
    if ( c_sizeof(i5) /= get_size_i5(i5) ) error stop 5
    if ( c_sizeof(i6) /= get_size_i6(i6) ) error stop 6
    if ( c_sizeof(i7) /= get_size_i7(i7) ) error stop 7
    if ( c_sizeof(i8) /= get_size_i8(i8) ) error stop 8
    if ( c_sizeof(i9) /= get_size_i9(i9) ) error stop 9
    if ( c_sizeof(i10) /= get_size_i10(i10) ) error stop 10
    if ( c_sizeof(i11) /= get_size_i11(i11) ) error stop 11
    if ( c_sizeof(i12) /= get_size_i12(i12) ) error stop 12
    if ( c_sizeof(i13) /= get_size_i13(i13) ) error stop 13
    if ( c_sizeof(i14) /= get_size_i14(i14) ) error stop 14
    if ( c_sizeof(i15) /= get_size_i15(i15) ) error stop 15
    if ( c_sizeof(i16) /= get_size_i16(i16) ) error stop 16
    if ( c_sizeof(i17) /= get_size_i17(i17) ) error stop 17
    if ( c_sizeof(i18) /= get_size_i18(i18) ) error stop 18
    if ( c_sizeof(i19) /= get_size_i19(i19) ) error stop 19
    if ( c_sizeof(i20) /= get_size_i20(i20) ) error stop 20

    if ( c_sizeof(r1) /= get_size_r1(r1) ) error stop 21
    if ( c_sizeof(r2) /= get_size_r2(r2) ) error stop 22
    if ( c_sizeof(r3) /= get_size_r3(r3) ) error stop 23

    if ( c_sizeof(c1) /= get_size_c1(c1) ) error stop 24
    if ( c_sizeof(c2) /= get_size_c2(c2) ) error stop 25
    if ( c_sizeof(c3) /= get_size_c3(c3) ) error stop 26

    if ( c_sizeof(ll) /= get_size_ll(ll) ) error stop 27
    if ( c_sizeof(cc) /= get_size_cc(cc) ) error stop 28
    if ( c_sizeof(cc2) /= get_size_cc(cc2) ) error stop 29

end program

