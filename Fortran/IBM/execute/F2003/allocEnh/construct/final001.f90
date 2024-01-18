!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/7/2006
!*
!*  DESCRIPTION                : allocatable enhancement
!                               Test the finalization process during the
!                               auto-reallocation process for the intrinsic
!                               assignment for allocatable enhancement.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer, allocatable :: ids(:)

        contains

        final :: finalizeBaseRank1
    end type

    contains

    subroutine finalizeBaseRank1 (b1)
        type(base), intent(inout) :: b1(:)

        print *, 'finalizeBaseRank1'

        print *, lbound(b1,1), ubound(b1,1)

        do i = lbound(b1,1), ubound(b1,1)
            if (allocated(b1(i)%ids)) deallocate(b1(i)%ids)
        end do
    end subroutine
end module

program final001
use m
    type(base), allocatable :: b1(:)

    !! there is no finalization at this point
    b1 = [(base(null()), i=1,100)]

    if (size(b1) /= 100) error stop 1_4

    do i = 1, 100
        if (allocated(b1(i)%ids)) error stop 2_4
    end do

    !! finalization happens during deallocation of b1 (then reallocated)
    b1 = b1(21:30)

    if (size(b1) /= 10) error stop 3_4

    do i = 1, 10
        if (allocated(b1(i)%ids)) error stop 4_4
    end do

    !! finalization happens for the assignment
    b1 = [(base([(j, j= 1, i)]), i=1,10)]

    if (size(b1) /= 10) error stop 5_4

    do i = 1, 10
        if (size(b1(i)%ids) /= i) error stop 6_4

        do j = 1, i
            if (b1(i)%ids(j) /= j) error stop 7_4
        end do
    end do
end
