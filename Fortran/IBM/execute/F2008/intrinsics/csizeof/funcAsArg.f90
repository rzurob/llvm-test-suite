! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2010-12-01
!*  ORIGIN                     :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : - test function results as actual arg of c_sizeof
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program main

    use, intrinsic :: iso_c_binding
    implicit none
    interface

        integer(C_SIZE_T) function get_size_r4(x) bind(c)
            use, intrinsic :: iso_c_binding
            real(c_float) x
        end function get_size_r4

        integer(C_SIZE_T) function get_size_i8(x) bind(c)
            use, intrinsic :: iso_c_binding
            integer(c_long_long) x(4)
        end function get_size_i8

        integer(C_SIZE_T) function get_size_c4(x) bind(c)
            use, intrinsic :: iso_c_binding
            complex(c_float_complex) x(2)
        end function get_size_c4

    end interface

    if ( c_sizeof(real4()) /= get_size_r4(real4()) ) error stop 21
    if ( c_sizeof(int8()) /= get_size_i8(int8()) ) error stop 31
    if ( c_sizeof(complex8()) /= get_size_c4(complex8()) ) error stop 41

    contains
        function real4()
            use, intrinsic :: iso_c_binding
            real(c_float)  real4
            real4 = 1
        end function

        function int8()
            use, intrinsic :: iso_c_binding
            integer(c_long_long) int8(4)
            int8 = 1
        end function

        function complex8()
            use, intrinsic :: iso_c_binding
            complex(c_float_complex) complex8(2)
            complex8 = 1
        end function

end program
