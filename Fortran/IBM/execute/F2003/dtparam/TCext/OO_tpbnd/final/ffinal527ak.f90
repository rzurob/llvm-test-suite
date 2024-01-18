! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : ffinal527ak
!*
!*  DATE                       : 2007-11-11 (original: 04/23/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : type bound
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DESCRIPTION                : final sub (elemental finalizer)
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

    elemental subroutine finalizeBase (b)
        type (base(4)), intent(inout) :: b ! tcx: (4)

        b%id = -1
    end subroutine

    subroutine finalizeBaseRank1 (b)
        type (base(4)), intent(in) :: b(:) ! tcx: (4)

        print *, 'finalizeBaseRank1'
    end subroutine
end module

program ffinal527ak
use m
    class (base(4)), allocatable :: b1(:,:) ! tcx: (4)

    allocate (b1(3,3))

    call abc (b1)

    if (allocated (b1)) error stop 101_4

    contains

    subroutine abc (b)
        class (base(4)), intent(out), allocatable :: b(:, :) ! tcx: (4)

        if (allocated (b)) error stop 102_4
    end subroutine
end


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 4 changes
