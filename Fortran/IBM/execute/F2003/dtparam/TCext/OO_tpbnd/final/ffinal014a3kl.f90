! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : ffinal014a3kl
!*
!*  DATE                       : 2007-10-31 (original: 06/21/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : type bound
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DESCRIPTION                : final sub (unlimited poly entities'
!                               finalization)
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

        final :: finalizeChild, finalizeChildRank1
    end type

    contains

    subroutine finalizeBase (b)
        type (base(4)) :: b ! tcx: (4)

        print *, 'finalizeBase'
    end subroutine

    subroutine finalizeChild (c)
        type (child(4,*)) :: c ! tcx: (4,0)

        print *, 'finalizeChild'
    end subroutine

    subroutine finalizeChildRank1 (c)
        type (child(4,*)) :: c(:) ! tcx: (4,0)

        print *, 'finalizeChildRank1 and then', size (c), 'base parent comp.'
    end subroutine
end module

program ffinal014a3kl
use m
    class (*), pointer :: x1, x2(:)
    class (*), allocatable :: x3, x4(:)

    allocate (child(4,0) :: x1, x3, x2(2), x4(3)) ! tcx: (4,0)

    print *, 'deallocating x1, x3'

    deallocate (x1, x3)

    print *, 'deallocating x2'

    deallocate (x2)

    print *, 'deallocating x4'

    deallocate (x4)

    print *, 'end'
end


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 1 changes
! type: child - added parameters (lChild) to invoke with (4,0) / declare with (4,*) - 3 changes
