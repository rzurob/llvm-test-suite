!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/8/2006
!*
!*  DESCRIPTION                : allocatable enhancement
!                               Test the finalization process during the
!                               intrinsic assignment for array sections of
!                               rank-one.
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

program final001a
use m
    type(base), allocatable :: b1(:)

    !! there is no finalization at this point
    b1 = [(base([(j, j=1,i)]), i=1,100)]

    if (size(b1) /= 100) error stop 1_4

    print *, '1'
    !! finalization happens during the intrinsic assignment
    b1(100:1:-2) = b1(::2)

    print *, 2

    do i = 1, 100, 2
        if (size(b1(i)%ids) /= i) error stop 2_4

        do j = 1, i
            if (b1(i)%ids(j) /= j) error stop 3_4
        end do

        if (size(b1(i+1)%ids) /= 100-i) error stop 4_4

        do j = 1, 100 - i
            if (b1(i+1)%ids(j) /= j) error stop 5_4
        end do
    end do
end
