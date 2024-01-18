! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2010-10-18
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : - test type(C_PTR) which is interoperable
!*                                 with C pointer type
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

        integer(C_SIZE_T) function get_ptrsize(x) bind(c)
            use, intrinsic :: iso_c_binding
            type(C_PTR) x
        end function get_ptrsize

    end interface

    integer, target :: arr(64)

    type(C_PTR) :: ptr

    ptr = C_LOC(arr)

    if ( c_sizeof(ptr) /= get_ptrsize(ptr) ) error stop 10

end

