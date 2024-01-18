! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-11-07 (original: 04/11/2005)
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DESCRIPTION                : final sub (finalization of temporaries created
!                               by structure constructor)
!*
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

program ffinal515_1k
use m
    type (base(4)) :: b(10) = base(4) (10)   !<-- this should not be finalized ! tcx: (4) ! tcx: (4)

    print *, 'begin'

    b = base(4) (20)    !<-- the temp here should be finalized before next stmt ! tcx: (4)

    print *, 'end'

    if (any(b%id /= 20)) error stop 101_4
end



! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 5 changes
