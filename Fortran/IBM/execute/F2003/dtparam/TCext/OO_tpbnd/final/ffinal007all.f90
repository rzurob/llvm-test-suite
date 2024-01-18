! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-10-31 (original: 02/09/2005)
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DESCRIPTION                : final sub (parent components finalizations in
!                               step 3; use array entities)
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
    type base (lBase) ! lBase=0
       integer, len :: lBase
        contains

        final :: finalizeBase
    end type

    type, extends(base) :: child (lchild_1) ! lchild_1=15
       integer, len :: lchild_1
        character(lchild_1) :: name
    end type

    contains

    subroutine finalizeBase (b)
        type (base(*)), intent(in) :: b ! tcx: (*)

        print *, 'finalizeBase'
    end subroutine
end module

program ffinal007all
use m
    type (child(:,:)), pointer :: c1(:,:) ! tcx: (:,:)

    allocate (child(0,15)::c1(2,2)) ! tcx: child(0,15)

    deallocate (c1)

    print *, 'end'
end


! Extensions to introduce derived type parameters:
! type: base - added parameters (lBase) to invoke with (0) / declare with (*) - 1 changes
! type: child - added parameters (lchild_1) to invoke with (0,15) / declare with (*,*) - 1 changes
