! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE NAME             : ffinal001a2k
!*
!*  PROGRAMMER                 : David Forster (derived from ffinal001a2 by Jim Xia)
!*  DATE                       : 2007-10-31 (original: 08/24/2004)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : type bound 
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DRIVER STANZA              : xlf2003 (original: xlf95)
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
    type base (kbase_1) ! kbase_1=4
       integer, kind :: kbase_1
        integer(kbase_1) :: flag

        contains

        final :: finalizeBase, finalizeBaseArray1
        final :: finalizeBaseArray2
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

    subroutine finalizeBaseArray2 (b)
        type (base(4)), intent(inout) :: b(:, :) ! tcx: (4)

        print *, 'finalizer for rank 2 array of base type'
    end subroutine

end module

program ffinal001a2k
use m
    type (base(4)), pointer :: b1_ptr (:) ! tcx: (4)
    type (base(4)), allocatable :: b1_alloc (:) ! tcx: (4)

    type (base(4)), pointer :: b2_ptr (:,:) ! tcx: (4)
    type (base(4)), allocatable :: b2_alloc (:,:) ! tcx: (4)

    allocate (b1_ptr(0), b1_alloc (-10:-11))
    allocate (b2_ptr (0:-1, 100), b2_alloc (10, 0))

    print *, size (b1_ptr), size (b1_alloc)
    print *, size (b2_ptr), size (b2_alloc)
    print *, associated (b1_ptr), associated(b2_ptr), allocated (b1_alloc), &
            allocated (b2_alloc)

    deallocate (b1_ptr, b2_ptr, b1_alloc, b2_alloc)
end


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 7 changes
