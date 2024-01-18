! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/29/2005
!*
!*  DESCRIPTION                : data pointer assignment (function return is a
!                               pointer and is a type bound and apply these on a
!                               named constant)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer data

        contains

        procedure :: returnDATA
    end type

    contains

    class(base) function returnData (b)
        pointer :: returnData
        class(base), intent(in) :: b

        allocate (returnData, source=b)
    end function
end module

use m
    type(base), parameter :: c1 = base(100)

    type (base), pointer :: c3

    nullify (c3)

    c3 => c1%returnData()

    if (.not. associated(c3)) error stop 1_4

    if (c3%data /= 100) error stop 2_4

    deallocate (c3)
end
