! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/21/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : data pointer assignment (deallocate the poly
!                               pointer will finalize the associated target)
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

module m
    type base
        integer*4 :: id
    end type

    type, extends(base) :: child
        class (base), pointer :: data => null()

        contains

        final :: finalizeChild
    end type

    class (child), pointer :: c1_m (:)
    type (child), pointer :: c2_m (:)

    contains

    subroutine finalizeChild (d)
        type (child), intent(inout) :: d (:)

        do i = 1, size (d)
            print *, 'checking data', i
            if (associated (d(i)%data)) then
                deallocate (d(i)%data)
            end if
        end do
    end subroutine

    subroutine allocateC2_m
        allocate (c2_m(2))
    end subroutine
end module

program fpAssgn023a2
use m
    class (base), pointer :: b_ptr (:)
    class (child), pointer :: c1 (:)

    allocate (c1(2:4))

    b_ptr => c1

    deallocate (b_ptr)

    allocate (c1_m(10))

    b_ptr => c1_m

    deallocate (b_ptr)

    call allocateC2_m

    c1 => c2_m
    b_ptr => c1

    deallocate (b_ptr)
end
