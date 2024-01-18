! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : ffinal509kl
!*
!*  DATE                       : 2007-10-12 (original: 04/01/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : type bound
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DESCRIPTION                : final sub (for intent(out), non-poly dummy-arg
!*                               associated with poly-actual-arg with different
!*                               dynamic type, then the finalization and default
!*                               initialization only applies to the part that is
!*                               associated with the dummy-arg)
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
        procedure :: print => printBase
    end type

    type, extends(base) :: child (lchild_1) ! lchild_1=20
       integer, len :: lchild_1
        character(lchild_1) :: name = ''

        contains

        procedure :: print => printChild
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

    subroutine printBase (b)
        class (base(4)), intent(in) :: b ! tcx: (4)

        print *, b%id
    end subroutine

    subroutine printChild(b)
        class (child(4,*)), intent(in) :: b ! tcx: (4,*)

        print *, b%id, b%name
    end subroutine
end module

program ffinal509kl
use m
    type (child(4,20)), target :: c1 ! tcx: (4,20)
    class (base(4)), pointer :: b_ptr ! tcx: (4)

    c1%name = 'c1'

    b_ptr => c1

    call sub (b_ptr)

    call b_ptr%print

    contains

    subroutine sub (b)
        type (base(4)), intent(out) :: b ! tcx: (4)

        b%id = 10
    end subroutine
end


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 4 changes
! type: child - added parameters (lchild_1) to invoke with (4,20) / declare with (4,*) - 3 changes
