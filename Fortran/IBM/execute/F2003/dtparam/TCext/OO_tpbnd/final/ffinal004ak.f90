! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE NAME             : ffinal004ak
!*
!*  PROGRAMMER                 : David Forster (derived from ffinal004a by Jim Xia)
!*  DATE                       : 2007-10-31 (original: 02/07/2005)
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*  DRIVER STANZA              : xlf2003
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
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
    type base (kbase_1) ! kbase_1=4
       integer, kind :: kbase_1
        integer(kbase_1) :: flag

        contains

        final :: finalizeBase, finalizeBaseArray1
        final :: finalizeBaseArray2, finalizeBaseArray3
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
        print *, 'shape of array: ', shape (b)
    end subroutine

    subroutine finalizeBaseArray3 (b)
        type (base(4)), intent(inout) :: b(:,:,:) ! tcx: (4)

        print *, 'finalizer for rank 3 array of base type'
        print *, 'shape of array: ', shape (b)
    end subroutine
end module

program ffinal004ak
use m

    type (base(4)), pointer :: b1_ptr ! tcx: (4)
    type (base(4)), allocatable :: b1_allo ! tcx: (4)

    type (base(4)), pointer :: b2_ptr(:) ! tcx: (4)
    type (base(4)), allocatable :: b2_allo (:) ! tcx: (4)

    type (base(4)), pointer :: b3_ptr(:,:) ! tcx: (4)
    type (base(4)), pointer :: b4_ptr(:,:,:) ! tcx: (4)

    type (base(4)), allocatable :: b3_allo(:,:) ! tcx: (4)
    type (base(4)), allocatable :: b4_allo(:,:,:) ! tcx: (4)

    !! The following two entities will not be finalized
    type (base(4)), pointer :: b5_ptr (:,:,:,:) ! tcx: (4)
    type (base(4)), allocatable :: b5_allo(:,:,:,:) ! tcx: (4)

    allocate (b1_ptr, b1_allo)

    allocate (b2_ptr(3), b2_allo(4))

    allocate (b3_ptr(2,2), b3_allo (3, 2))

    allocate (b4_ptr (1,1,1), b4_allo (1,1,1))

    allocate (b5_ptr (2, 3, 4, 5), b5_allo (5, 4, 2, 3))

    deallocate (b1_ptr, b2_ptr, b3_ptr, b4_ptr, b5_ptr)

    print *, 'end'
end


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 14 changes
