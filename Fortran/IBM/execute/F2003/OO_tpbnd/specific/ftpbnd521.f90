!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/02/2005
!*
!*  DESCRIPTION                : specific type bound (elemental functions and
!                               subroutines)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        real(8), pointer :: r1(:) => null()

        contains

        procedure :: copy => deepCopy
        final :: finalizeBase
    end type

    contains

    elemental subroutine finalizeBase (b)
        type(base), intent(inout) :: b

        if (associated (b%r1)) deallocate (b%r1)
    end subroutine

    elemental function deepCopy (b) result(retVal)
        class (base), intent(in) :: b

        type (base) retVal

        if (associated (b%r1)) then
            allocate (retVal%r1(size(b%r1)), source=b%r1)
        end if
    end function
end module

program ftpbnd521
use m
    class (base), allocatable :: b1(:)

    allocate (b1(10))

    associate (x => b1)
        do i = 1, 10
            allocate (x(i)%r1(i), source=(/(j*1.0_8, j =1,i)/))
        end do
    end associate

    associate (x => b1%copy())
        do i = 1, 10
            write (*, '(10f8.2)') x(i)%r1
        end do
    end associate
end
