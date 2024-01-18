! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            : implicitTyp.f 
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
!*  DESCRIPTION                : - test implicit type for interge,real, complex 
!*                                
!*                               - use -qintsize=2 -qrealsize=4
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
    implicit none
    real       r4
    integer    i2
    complex    c4 

    type, bind(c) :: dType
        integer   i2(3) 
        complex   c4(2) 
        real      r4 
    end type

end module

program main

    use mod

    interface
        integer(C_SIZE_T) function get_size_dt(x) bind(c)
            use, intrinsic :: iso_c_binding
            use mod , only : dType
            type(dType)  x
        end function get_size_dt

        integer(C_SIZE_T) function get_size_i2(x) bind(c)
            use, intrinsic :: iso_c_binding
            integer x 
        end function get_size_i2

        integer(C_SIZE_T) function get_size_r4(x) bind(c)
            use, intrinsic :: iso_c_binding
            real x
        end function get_size_r4

        integer(C_SIZE_T) function get_size_c4(x) bind(c)
            use, intrinsic :: iso_c_binding
            complex x 
        end function get_size_c4
 
    end interface 

    type(dType) :: dt

    !print *, sizeof(i2),sizeof(r4),sizeof(c4),sizeof(dt)
    !print *, get_size_i2(i2),get_size_r4(r4),get_size_c4(c4),get_size_dt(dt) 

    if ( c_sizeof(i2) /= get_size_i2(i2) ) error stop 21
    if ( c_sizeof(r4) /= get_size_r4(r4) ) error stop 31 
    if ( c_sizeof(c4) /= get_size_c4(c4) ) error stop 41
    if ( c_sizeof(dt) /= get_size_dt(dt) ) error stop 51

 
end program
