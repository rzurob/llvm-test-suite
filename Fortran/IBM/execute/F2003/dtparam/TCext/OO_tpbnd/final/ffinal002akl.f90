! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : ffinal002akl
!*
!*  DATE                       : 2007-10-31 (original: 5/10/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : type bound
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
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

program ffinal002akl

use m

    type, extends (base) :: child (lchild_1) ! lchild_1=20
       integer, len :: lchild_1
        character(lchild_1) :: name
    end type

    type (child(4,:)), pointer :: c1_ptr ! tcx: (4,:)
    type (child(4,:)), pointer :: c2_ptr (:) ! tcx: (4,:)
    type (child(4,:)), allocatable :: c3 (:) ! tcx: (4,:)

    allocate (child(4,20)::c1_ptr, c2_ptr(4), c3 (10)) ! tcx: child(4,20)

    deallocate (c1_ptr, c2_ptr, c3)
end


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 2 changes
! type: child - added parameters (lchild_1) to invoke with (4,20) / declare with (4,*) - 3 changes
