! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-10-31 (original: 06/21/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : type bound
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DESCRIPTION                : final sub (poly-allocatable's finalization)
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

    type, extends(base) :: child (kChild) ! kChild=4
       integer, kind :: kChild
        contains

        final :: finalizeChild, finalizeChildRank1
    end type

    contains

    subroutine finalizeBase (b)
        type (base(4)) :: b ! tcx: (4)

        print *, 'finalizeBase'
    end subroutine

    subroutine finalizeChild (c)
        type (child(4,4)) :: c ! tcx: (4,4)

        print *, 'finalizeChild'
    end subroutine

    subroutine finalizeChildRank1 (c)
        type (child(4,4)) :: c(:) ! tcx: (4,4)

        print *, 'finalizeChildRank1 and then', size (c), 'base parent comp.'
    end subroutine
end module

program ffinal014a2kk
use m
    type (child(4,4)), allocatable :: b (:) ! tcx: (4,4)
    class (base(4)), allocatable :: a1, a2 (:) ! tcx: (4)

    allocate (child(4,4) :: b(2), a1, a2(3)) ! tcx: (4,4)

    print *, 'deallocating b'

    deallocate (b)

    print *, 'deallocating a1'

    deallocate (a1)

    print *, 'deallocating a2'

    deallocate (a2)

    print *, 'end'
end


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 2 changes
! type: child - added parameters (kChild) to invoke with (4,4) / declare with (4,4) - 4 changes
