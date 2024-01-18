! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : ffinal007kl
!*
!*  DATE                       : 2007-10-31 (original: 02/09/2005)
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DESCRIPTION                : final sub (finalization of the parent
!                               components)
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
    type base (kBase) ! kBase=4
       integer, kind :: kBase
        contains

        final :: finalizeBase
    end type

    type, extends(base) :: child (lchild_1) ! lchild_1=15
       integer, len :: lchild_1
        character(lchild_1) :: name
    end type

    contains

    subroutine finalizeBase (b)
        type (base(4)), intent(in) :: b ! tcx: (4)

        print *, 'finalizeBase'
    end subroutine
end module

program ffinal007kl
use m
    type (child(4,:)), pointer :: c1 ! tcx: (4,:)

    allocate (child(4,15)::c1) ! tcx: child(4,15)

    deallocate (c1)

    print *, 'end'
end


! Extensions to introduce derived type parameters:
! type: base - added parameters (kBase) to invoke with (4) / declare with (4) - 1 changes
! type: child - added parameters (lchild_1) to invoke with (4,15) / declare with (4,*) - 1 changes
