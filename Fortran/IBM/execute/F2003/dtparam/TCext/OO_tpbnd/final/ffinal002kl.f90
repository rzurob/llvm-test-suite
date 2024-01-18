! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-10-31 (original: 04/26/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : type bound
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DESCRIPTION                : final sub (parent component is finalized)
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

        final :: finalizeBase
    end type

    type, extends (base) :: child (lchild_1) ! lchild_1=20
       integer, len :: lchild_1
        character(lchild_1) :: name

        contains

        final :: finalizeChild
    end type

    contains

    subroutine finalizeBase (b)
        type (base(4)), intent(inout) :: b ! tcx: (4)

        print *, 'finalizeBase'
    end subroutine

    subroutine finalizeChild (c)
        type (child(4,*)), intent(inout) :: c ! tcx: (4,*)

        print *, 'finalizeChild'
    end subroutine
end module

program ffinal002kl
use m
    type (child(4,:)), pointer :: c_ptr ! tcx: (4,:)

    allocate (child(4,20)::c_ptr) ! tcx: child(4,20)

    deallocate (c_ptr)
end


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 1 changes
! type: child - added parameters (lchild_1) to invoke with (4,20) / declare with (4,*) - 2 changes
