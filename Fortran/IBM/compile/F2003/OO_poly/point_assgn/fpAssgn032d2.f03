! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/29/2005
!*
!*  DESCRIPTION                : data pointer assignment (non-compatible types
!                               involved in the data pointer assignment)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        class(*), pointer :: data

        contains

        procedure :: returnDATA
    end type

    contains

    class(*) function returnData (b)
        pointer :: returnData
        class(base), intent(in) :: b

        returnData => b%data
    end function
end module

program fpAssgn032d2
use m
    type(base), parameter :: c1 = base(null())

    type (base), pointer :: c3

    c3 => c1%returnData()       !<-- illegal
end