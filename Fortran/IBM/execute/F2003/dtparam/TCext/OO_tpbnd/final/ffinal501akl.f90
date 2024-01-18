! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-11-01 (original: 02/10/2005)
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DESCRIPTION                : final sub (finalization of LHS during intrinsic
!                               assignment)
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
        integer(kbase_1) id

        contains

        final :: finalizeBase
    end type

    type, extends(base) :: child (lchild_1) ! lchild_1=20
       integer, len :: lchild_1
        character(lchild_1) :: name

        contains

        final :: finalizeChild
    end type

    contains

    subroutine finalizeBase (b)
        type (base(4)), intent(inout) :: b ! tcx: (4)

        print *, 'finalizeBase'

        b%id = 0
    end subroutine

    subroutine finalizeChild (c)
        type (child(4,*)), intent(inout) :: c ! tcx: (4,*)

        print *,'finalizeChild'

        c%name = ''
    end subroutine
end module

program ffinal501akl
use m
    type (child(4,20)) :: c1 ! tcx: (4,20)
    type (base(4)) :: b1 ! tcx: (4)

    b1%id = 1

    c1%id = 10
    c1%name = 'c1_test'

    c1%base = b1   !<-- this should print out finalizeBase

    if (c1%id /= 1) error stop 101_4

    print *, 'end'
end


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 2 changes
! type: child - added parameters (lchild_1) to invoke with (4,20) / declare with (4,*) - 2 changes
