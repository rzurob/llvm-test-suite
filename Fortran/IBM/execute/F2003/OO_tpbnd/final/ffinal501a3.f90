!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/23/2005
!*
!*  DESCRIPTION                : final sub (finalization of LHS during the
!                               intrinsic assignment)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer, allocatable :: i1

        contains

        final :: finalizeBase, finalizeBaseRank1
    end type

    contains

    subroutine finalizeBase (b)
        type (base), intent(inout) :: b

        print *, 'finalizeBase'
    end subroutine

    subroutine finalizeBaseRank1 (b)
        type (base), intent(inout) :: b(:)

        print *, 'finalizeBaseRank1'
    end subroutine
end module

program ffinal501a3
use m
    type (base) b1(3), b3

    allocate (b3%i1, source=10)

    b1 = b3

    if ((b1(1)%i1 /= 10) .or. (b1(2)%i1 /= 10) .or.&
        (b1(3)%i1 /= 10)) error stop 1_4

    print *, 'end'
end
