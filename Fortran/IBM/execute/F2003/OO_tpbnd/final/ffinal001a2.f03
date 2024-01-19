! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 08/24/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : final sub (zero-size pointer/allocatable
!                               arrays)
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
        final :: finalizeBaseArray2
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

    subroutine finalizeBaseArray2 (b)
        type (base), intent(inout) :: b(:, :)

        print *, 'finalizer for rank 2 array of base type'
    end subroutine

end module

program ffinal001a2
use m
    type (base), pointer :: b1_ptr (:)
    type (base), allocatable :: b1_alloc (:)

    type (base), pointer :: b2_ptr (:,:)
    type (base), allocatable :: b2_alloc (:,:)

    allocate (b1_ptr(0), b1_alloc (-10:-11))
    allocate (b2_ptr (0:-1, 100), b2_alloc (10, 0))

    print *, size (b1_ptr), size (b1_alloc)
    print *, size (b2_ptr), size (b2_alloc)
    print *, associated (b1_ptr), associated(b2_ptr), allocated (b1_alloc), &
            allocated (b2_alloc)

    deallocate (b1_ptr, b2_ptr, b1_alloc, b2_alloc)
end
