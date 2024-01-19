! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-10-16 (original: 04/14/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : type bound
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DESCRIPTION                : final sub (finalization happens for arrays
!*                               associated with intent(out) dummy-arg)
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
        final :: finalizeBaseRank1
    end type

    contains

    subroutine finalizeBase (b)
        type (base(4)), intent(in) :: b ! tcx: (4)

        print *, 'finalizeBase'
    end subroutine

    subroutine finalizeBaseRank1 (b)
        type(base(4)) b(:) ! tcx: (4)

        print *, 'finalizeBaseRank1'
    end subroutine

    subroutine abc (b)
        type (base(4)), intent(out) :: b(:) ! tcx: (4)

        print *, 'abc'
    end subroutine
end module

program ffinal513a4k
use m
    type (base(4)), save :: b1(2) ! tcx: (4)

    call abc (b1)
end


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 4 changes
