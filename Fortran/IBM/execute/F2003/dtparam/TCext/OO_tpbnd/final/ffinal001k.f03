! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-10-31 (original: 08/13/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : type bound
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
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

    type base (kbase_1) ! kbase_1=4
       integer, kind :: kbase_1
        integer(kbase_1) :: flag

        contains

        final :: finalizeBase, finalizeBaseArray1
    end type

    contains

    subroutine finalizeBase (b)
        type (base(4)), intent(inout) :: b ! tcx: (4)

        print *, 'finalizeBase'
    end subroutine

    subroutine finalizeBaseArray1 (b)
        type (base(4)), intent(inout) :: b(:) ! tcx: (4)

        print *, 'finalizer for rank 1 array of base type'
        print *, 'size of the finalized array is:', size(b)
    end subroutine
end module

program ffinal001k
use m

    type (base(4)), pointer :: b1_ptr ! tcx: (4)
    type (base(4)), pointer :: b2_ptr(:) ! tcx: (4)
    type (base(4)), allocatable :: b3 (:) ! tcx: (4)

    allocate (b1_ptr, b2_ptr(4), b3 (10))

    deallocate (b1_ptr, b2_ptr, b3)
end


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 5 changes
