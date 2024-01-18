! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-10-12 (original: 04/12/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : type bound
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DESCRIPTION                : final sub (all temps in an array constructor
!*                               shall be finalized; temps are created by
!*                               structure constructor)
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
        integer(kbase_1) id

        contains

        final :: finalizeBase, finalizeBaseRank1
    end type

    contains

    subroutine finalizeBase (b)
        type (base(4)), intent(in) :: b ! tcx: (4)

        print *, 'finalizeBase'
    end subroutine

    subroutine finalizeBaseRank1 (b)
        type (base(4)), intent(in) ::b(:) ! tcx: (4)

        print *, 'finalizeBaseRank1'
    end subroutine
end module

use m
    type (base(4)), save :: b(3) ! tcx: (4)

    b = (/(base(4)(i), i=1,3)/) ! tcx: (4)

    print *, 'end'
end



! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 4 changes
