! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/07/2005
!*
!*  DESCRIPTION                : final subroutine (explict-shape array as the
!                               dummy-arg for the final sub)
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
    end type

    contains

    subroutine finalizeBase (b)
        type (base), intent(inout) :: b

        print *, 'finalizeBase'
    end subroutine

    subroutine finalizeBaseArray1 (b)
        type (base), intent(inout) :: b(2)   ! this is an explicit-shape

        print *, 'finalizer for rank 1 array of base type'
        print *, b%flag
    end subroutine

end module

program ffinal004a1
use m

    type (base), pointer :: b1_ptr
    type (base), allocatable :: b1_allo

    type (base), pointer :: b2_ptr(:)
    type (base), allocatable :: b2_allo (:)

    allocate (b1_ptr, b1_allo)

    allocate (b2_ptr(3), b2_allo(4))

    b2_ptr%flag = (/1,2,3/)

    deallocate (b1_ptr, b2_ptr)

    print *, 'end'
end
