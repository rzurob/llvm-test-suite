!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/21/2006
!*
!*  DESCRIPTION                : miscellaneous (defect 316115)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type A
        integer(8) i

        contains

        procedure :: getVal
    end type

    contains

    function getVal(a1)
        class(A), intent(in) :: a1
        real(kind(a1%i)) getVal

        getVal = a1%i
    end function
end module

program misc006a
use m
    type(A), allocatable :: a1

    allocate (a1, source=A(100))

    if (a1%getVal() /= 100) error stop 1_4
end
