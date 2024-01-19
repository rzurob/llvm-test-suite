! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-10-31 (original: 08/24/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : type bound
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DESCRIPTION                : final sub (C1273: Any procedure referenced in a
!                               pure subprogram, including one referenced via a
!                               defined-operation, assignment, or finalization,
!                               shall be pure)
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

    interface assignment (=)
        pure subroutine asgnB2ToB1 (b1, b2)
        import base
            class (base(4)), intent(out) :: b1 ! tcx: (4)
            type (base(4)), intent(in) :: b2 ! tcx: (4)
        end subroutine
    end interface

    contains

    subroutine finalizeBase (b)
        type (base(4)), intent(in) :: b ! tcx: (4)

        print *, 'finalizeBase'
    end subroutine
end module


!! there is one reference to finalizeBase for b1, which has INTENT(OUT) attr.
pure subroutine asgnB2ToB1 (b1, b2)
use m, only: base
    class (base(4)), intent(out) :: b1 ! tcx: (4)
    type (base(4)), intent(in) :: b2 ! tcx: (4)

    b1%id = b2%id
end subroutine


!! there are three refencese to finalizeBase; only two will get caught here
pure subroutine invalid
use m
    type (base(4)) :: b1, b2   !<-- this will generate 2 calls to finalizeBase ! tcx: (4)

    b1 = b2  !<-- since asgnB2ToB1 fails to compile, this will not be caught
end subroutine

program ffinal001d2k
    call invalid
end


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 6 changes
