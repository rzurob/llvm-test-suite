! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-10-31 (original: 06/21/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : type bound
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DESCRIPTION                : final sub (poly-pointer's finalization)
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
        integer(kbase_1) :: id

        contains

        final :: finalizeBase
    end type

    type, extends(base) :: child (lChild) ! lChild=0
       integer, len :: lChild
        contains

        final :: finalizeChild
    end type

    contains

    subroutine finalizeBase (b)
        type (base(4)) :: b ! tcx: (4)

        print *, 'finalizeBase'
    end subroutine

    subroutine finalizeChild (c)
        type (child(4,*)) :: c ! tcx: (4,0)
        print *, 'finalizeChild', c%id
    end subroutine
end module

program ffinal014akl
use m
    type (base(4)), pointer :: b_ptr ! tcx: (4)
    type (child(4,:)), pointer :: c_ptr ! tcx: (4,:)

    class (base(4)), pointer :: b ! tcx: (4)

    allocate (b_ptr)
    allocate (child(4,0):: c_ptr) ! tcx: child(4,0)

    b_ptr%id = 1
    c_ptr%id = 10

    b => c_ptr

    deallocate (b)

    print *, 're-assign pointer b'

    b => b_ptr

    deallocate (b)
end


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 3 changes
! type: child - added parameters (lChild) to invoke with (4,0) / declare with (4,*) - 2 changes