! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : ffinal513a6k
!*
!*  DATE                       : 2007-10-31 (original: 05/03/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : type bound
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DESCRIPTION                : final sub (finalization of allocatable actual arg
!*                               asssociated with INTENT(OUT) dummy-arg)
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

        final :: finalizeBase, finalizeBaseRank1
    end type

    contains

    subroutine finalizeBase (b)
        type (base(4)), intent(in) :: b ! tcx: (4)

        print *, 'finalizeBase'
    end subroutine

    subroutine finalizeBaseRank1 (b)
        type (base(4)), intent(in) :: b(:) ! tcx: (4)

        print *, 'finalizeBaseRank1'
    end subroutine

    subroutine abc (b)
        type (base(4)), allocatable, intent(out) :: b ! tcx: (4)
    end subroutine

    subroutine cba (b)
        type (base(4)), allocatable, intent(out) :: b(:) ! tcx: (4)
    end subroutine
end module

program ffinal513a6k
use m
    type (base(4)), allocatable :: b1, b2(:) ! tcx: (4)

    allocate (b1, b2(2))

    call abc (b1)

    print *, 'test array'

    call cba (b2)

    print *, 'end'
end


! Extensions to introduce derived type parameters:
! type: child - added parameters (kchild_1) to invoke with (4) / declare with (4) - 3 changes
! type: child - added parameters (kchild_1) to invoke with (4) / declare with (4) - 3 changes
! type: child - added parameters (kchild_1) to invoke with (4) / declare with (4) - 3 changes
! type: child - added parameters (kchild_1) to invoke with (4) / declare with (4) - 3 changes
! type: child - added parameters (kchild_1) to invoke with (4) / declare with (4) - 3 changes
! type: child - added parameters (kchild_1) to invoke with (4) / declare with (4) - 3 changes
! type: child - added parameters (kchild_1) to invoke with (4) / declare with (4) - 3 changes
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 5 changes
