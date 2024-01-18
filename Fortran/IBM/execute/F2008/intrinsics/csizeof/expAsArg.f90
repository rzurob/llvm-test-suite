! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            : expAsArg.f 
!*
!*  PROGRAMMER                 : Michelle Zhang 
!*  DATE                       : 2010-12-01
!*  ORIGIN                     :
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              :
!*
!*  DESCRIPTION                : - test expressions as actual arg of c_sizeof 
!*                               - test literal as actual arg of c_sizeof
!*                               - test literal defined by parameter attr as actual arg of c_sizeof
!*                               - test argument has default type and parameter
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

    integer(c_long)           iA
    integer(c_long_long)      iB(4)

    real(c_float)             rA

    type, bind(c) :: dType
        complex(c_float_complex)   c4(2) 
    end type


    interface

        integer(C_SIZE_T) function get_size_r4(x) bind(c)
            use, intrinsic :: iso_c_binding
            real(c_float) x 
        end function get_size_r4

        integer(C_SIZE_T) function get_size_r8(x) bind(c)
            use, intrinsic :: iso_c_binding
            real(c_double) x 
        end function get_size_r8

        integer(C_SIZE_T) function get_size_i2(x) bind(c)
            use, intrinsic :: iso_c_binding
            integer(c_short) x
        end function get_size_i2

        integer(C_SIZE_T) function get_size_i8(x) bind(c)
            use, intrinsic :: iso_c_binding
            integer(c_long_long) x(4)
        end function get_size_i8

        integer(C_SIZE_T) function get_size_c4(x) bind(c)
            use, intrinsic :: iso_c_binding
            complex(c_float_complex) x(2) 
        end function get_size_c4
 
    end interface 

    type(dType) :: dt

    integer*2, parameter :: ab = 2

    !print *, sizeof(iA+RA),sizeof(kind(iB)*iB),sizeof(dt%c4)
    !print *, get_size_r4(iA+RA),get_size_i8(kind(iB)*iB),get_size_c4(dt%c4)

    ! argument of c_sizeof is expression
    if ( c_sizeof(iA+rA) /= get_size_r4(iA+rA) ) error stop 21
    if ( c_sizeof(kind(iB)*iB) /= get_size_i8(kind(iB)*iB) ) error stop 31 
    if ( c_sizeof(dt%c4) /= get_size_c4(dt%c4) ) error stop 41

    ! argument of c_sizeof is literal
    if ( c_sizeof(ab) /= get_size_i2(ab) ) error stop 51
    if ( c_sizeof(2.0_8) /= get_size_r8(2.0_8) ) error stop 61

    ! argument of c_sizeof is default real type and paramenter
    if ( c_sizeof(4.0) /= get_size_r4(4.0) ) error stop 71

 
end program
