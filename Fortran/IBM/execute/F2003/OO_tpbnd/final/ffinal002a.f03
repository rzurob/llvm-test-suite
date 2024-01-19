! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 5/10/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : final sub (parent component finalized in step
!*                               3)
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

program ffinal002a

use m

    type, extends (base) :: child
        character(20) :: name
    end type

    type (child), pointer :: c1_ptr
    type (child), pointer :: c2_ptr (:)
    type (child), allocatable :: c3 (:)

    allocate (c1_ptr, c2_ptr(4), c3 (10))

    deallocate (c1_ptr, c2_ptr, c3)
end
