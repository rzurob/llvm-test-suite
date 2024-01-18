! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2010-10-18
!*  ORIGIN                     :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : - derived-type components are scalars of
!*                                 type real with all named constants
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
    type, bind(c) :: dTypeA
         real(C_FLOAT)  :: a(8)
         real(C_DOUBLE)  :: b(4)
         real(C_LONG_DOUBLE)  :: c
    end type dTypeA

    type, bind(c) :: dTypeA1
         real(C_LONG_DOUBLE)  :: b
    end type dTypeA1


    interface

        integer(C_SIZE_T) function get_sizeA(x) bind(c)
            use, intrinsic :: iso_c_binding
            import dTypeA
            type(dTypeA) x
        end function get_sizeA

        integer(C_SIZE_T) function get_sizeA1(x) bind(c)
            use, intrinsic :: iso_c_binding
            import dTypeA1
            type(dTypeA1) x
        end function get_sizeA1
    end interface

    type(dTypeA) :: dt1
    type(dTypeA1) :: dt2

    if ( c_sizeof(dt1) /= get_sizeA(dt1) ) error stop 10
    if ( c_sizeof(dt2) /= get_sizeA1(dt2) ) error stop 20

end

