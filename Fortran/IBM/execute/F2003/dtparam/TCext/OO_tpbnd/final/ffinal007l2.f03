! *********************************************************************
!*  ===================================================================
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
    type base (lBase) ! lBase=15
       integer, len :: lBase
        contains

        final :: finalizeBase
    end type

    type, extends(base) :: child
        character(lBase) :: name
    end type

    contains

    subroutine finalizeBase (b)
        type (base(*)), intent(in) :: b ! tcx: (*)

        print *, 'finalizeBase'
    end subroutine
end module

program ffinal007l2
use m
    type (child(:)), pointer :: c1 ! tcx: (:)

    allocate (child(15)::c1) ! tcx: child(15)

    deallocate (c1)

    print *, 'end'
end


! Extensions to introduce derived type parameters:
! type: base - added parameters (lBase) to invoke with (15) / declare with (*) - 1 changes