! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 08/13/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : final sub (basic test on final subroutine on
!                               scalar and rank-one arrays)
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
        integer*4 :: flag

        contains

        final :: finalizeBase, finalizeBaseArray1
    end type

    contains

    subroutine finalizeBase (b)
        type (base), intent(inout) :: b

        print *, 'finalizeBase'
    end subroutine

    subroutine finalizeBaseArray1 (b)
        type (base), intent(inout) :: b(:)

        print *, 'finalizer for rank 1 array of base type'
        print *, 'size of the finalized array is:', size(b)
    end subroutine
end module

program ffinal001
use m

    type (base), pointer :: b1_ptr
    type (base), pointer :: b2_ptr(:)
    type (base), allocatable :: b3 (:)

    allocate (b1_ptr, b2_ptr(4), b3 (10))

    deallocate (b1_ptr, b2_ptr, b3)
end
