! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-11-01 (original: 04/27/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : type bound
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DESCRIPTION                : final sub (variables not finalized due to
!*                               execution of END PROGRAM)
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
        integer(kbase_1) :: id = -1

        contains

        final :: finalizeBase
    end type

    contains

    subroutine finalizeBase (b)
        type (base(4)), intent(in) :: b ! tcx: (4)

        print *, 'finalizeBase'
    end subroutine

end module

module m1
use m
    type, extends (base) :: child (lChild) ! lChild=0
       integer, len :: lChild

        contains
        final :: finalizeChild
    end type

    contains

    subroutine finalizeChild (c)
        type (child(4,*)), intent(in) :: c ! tcx: (4,*)

        print *, 'finalizeChild'
    end subroutine
end module

program ffinal502akl
use m1
    type (child(4,0)) :: c1 ! tcx: (4,0)

    print *, 'end'
end


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 1 changes
! type: child - added parameters (lChild) to invoke with (4,0) / declare with (4,*) - 2 changes
