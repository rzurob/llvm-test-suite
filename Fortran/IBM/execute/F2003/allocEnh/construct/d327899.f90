!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/8/2006
!*
!*  DESCRIPTION                : miscellaneous (defect 327899)
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

use m
    type(base), allocatable :: b1(:)

    !! there is no finalization at this point
    allocate (b1(100))

    do i = 1, 100
        allocate(b1(i)%ids(i), source=i)
    end do

    print *, '1'
    !! finalization happens during deallocation of b1 (then reallocated)
    b1(100:1:-2) = b1(::2)

    print *, '2'

    do i = 1, 100, 2
        if ((size(b1(i)%ids) /= i) .or. (size(b1(i+1)%ids) /= 100-i)) &
            error stop 1_4

        do j = 1, i
            if (b1(i)%ids(j) /= i) error stop 2_4
        end do

        do j = 1, 100-i
            if (b1(i+1)%ids(j) /= 100-i) error stop 3_4
        end do
    end do
end
