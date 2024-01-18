! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-11-01 (original: 04/23/2005)
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DESCRIPTION                : final sub (finalization of LHS during the
!                               intrinsic assignment)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (kbase_1) ! kbase_1=4
       integer, kind :: kbase_1
        integer(kbase_1), allocatable :: i1

        contains

        final :: finalizeBase, finalizeBaseRank1
    end type

    contains

    subroutine finalizeBase (b)
        type (base(4)), intent(inout) :: b ! tcx: (4)

        print *, 'finalizeBase'
    end subroutine

    subroutine finalizeBaseRank1 (b)
        type (base(4)), intent(inout) :: b(:) ! tcx: (4)

        print *, 'finalizeBaseRank1'
    end subroutine
end module

program ffinal501a3k
use m
    type (base(4)) b1(3), b3 ! tcx: (4)

    allocate (b3%i1, source=10)

    b1 = b3

    if ((b1(1)%i1 /= 10) .or. (b1(2)%i1 /= 10) .or.&
        (b1(3)%i1 /= 10)) error stop 101_4

    print *, 'end'
end


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 3 changes
