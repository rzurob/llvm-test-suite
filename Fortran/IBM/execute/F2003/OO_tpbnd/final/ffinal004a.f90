!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/07/2005
!*
!*  DESCRIPTION                : final subroutine (multi-dimensional arrays
!                               finalization)
!*
!*  KEYWORD(S)                 :
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
        final :: finalizeBaseArray2, finalizeBaseArray3
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
        print *, 'shape of array: ', shape (b)
    end subroutine

    subroutine finalizeBaseArray3 (b)
        type (base), intent(inout) :: b(:,:,:)

        print *, 'finalizer for rank 3 array of base type'
        print *, 'shape of array: ', shape (b)
    end subroutine
end module

program ffinal004a
use m

    type (base), pointer :: b1_ptr
    type (base), allocatable :: b1_allo

    type (base), pointer :: b2_ptr(:)
    type (base), allocatable :: b2_allo (:)

    type (base), pointer :: b3_ptr(:,:)
    type (base), pointer :: b4_ptr(:,:,:)

    type (base), allocatable :: b3_allo(:,:)
    type (base), allocatable :: b4_allo(:,:,:)

    !! The following two entities will not be finalized
    type (base), pointer :: b5_ptr (:,:,:,:)
    type (base), allocatable :: b5_allo(:,:,:,:)

    allocate (b1_ptr, b1_allo)

    allocate (b2_ptr(3), b2_allo(4))

    allocate (b3_ptr(2,2), b3_allo (3, 2))

    allocate (b4_ptr (1,1,1), b4_allo (1,1,1))

    allocate (b5_ptr (2, 3, 4, 5), b5_allo (5, 4, 2, 3))

    deallocate (b1_ptr, b2_ptr, b3_ptr, b4_ptr, b5_ptr)

    print *, 'end'
end
