! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/09/2006
!*
!*  DESCRIPTION                : allocatable enhancement
!                               Finalization of function results after the
!                               intrinsic assignment; test rank-one
!                               non-allocatable non-pointer array function
!                               result.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer, allocatable :: data(:)

        contains

        final :: finalizeBaseRank1
        procedure :: makeArray => makeBaseArray
    end type

    contains

    subroutine finalizeBaseRank1 (b1)
        type(base), intent(in) :: b1(:)

        print *, 'finalizeBaseRank1:', lbound(b1), ubound(b1)

        do i = lbound(b1, 1), ubound(b1, 1)
            if (allocated(b1(i)%data)) then
                print *, 'element', i, ':', lbound(b1(i)%data), &
                        ubound(b1(i)%data)
            end if
        end do
    end subroutine

    function makeBaseArray (b1)
        class(base), intent(in) :: b1

        type(base) makeBaseArray(size(b1%data))

        if (.not. allocated(b1%data)) error stop 100

        do i = 1, size(b1%data)
            makeBaseArray(i)%data = b1%data(lbound(b1%data,1): &
                lbound(b1%data,1)+i-1)
        end do
    end function
end module

program funcRetFinal001
use m
    type(base), allocatable :: b1(:)

    print *, 1
    !! initial allocation for b1
    allocate (b1(0:9))

    do i = 0, 9, 2
        b1(i)%data = [(j, j = 1, 10-i)]

        allocate (b1(i+1)%data(0:8-i), source = [(j, j = 1, 9-i)])
    end do

    !! test 2: 2 finalizations to happen for the intrinsic assignment: finalize
    !b1 when deallocating b1; then after the assignment, the function result is
    !finalized.
    print *, 2
    b1 = b1(3)%makeArray()

    print *, 'end'

    if ((lbound(b1,1) /= 1) .or. (ubound(b1,1) /= 7)) error stop 1_4

    do i = 1, 7
        if (size(b1(i)%data) /= i) error stop 3_4

        do j = 1, i
            if (b1(i)%data(j) /= j) error stop 2_4
        end do
    end do
end
