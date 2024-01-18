! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2010-08-10
!*  ORIGIN                     :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : This test case is designed to test the
!                               use of coarray and co-indexed objects within
!                               intrinsic function sum: data type is integer
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
    implicit none
    integer, parameter :: msize = 100

    integer(8), save :: arr(msize)[*]
    integer(8), save :: arrSum[*]
    integer(8), save :: arrSumRight[*]

    contains

    ! set up values for arr using external input
    ! this routine should be called from only one image
    subroutine setArr (x)
        integer(8), intent(in) :: x(:)
        integer :: np, i

        np = num_images()

        if (size(x) < msize*np) then
            print *, 'size of input data is too small'
            error stop 1
        end if

        do i = 1, num_images()
            arr(:)[i] = x((i-1)*msize+1 : i*msize)
        end do
    end subroutine

    ! compute arrSum using intrinsic function sum
    subroutine computeLocal
        arrSum = sum (arr)
    end subroutine

    ! compute arrSumRight: sum of arr(:)[right]
    subroutine computeRight
        integer me, right
        me = this_image()

        if (me == num_images()) then
            right = 1
        else
            right = me + 1
        end if

        arrSumRight = sum(arr(:)[right])
    end subroutine
end module

program sumCoarray01
use m
    implicit none
    integer(8), allocatable :: x(:)
    integer :: np, me, i, right
    np = num_images()

    me = this_image()

    if (me == num_images()) then
        right = 1
    else
        right = me + 1
    end if

    ! test 1, set value from 1 to np*msize
    if (me == 1) then
        x = [(i, i = 1,np*msize)]
        call setArr (x)
    end if

    sync all
    call computeLocal
    call computeRight
    sync all

    ! verify test 1 results
    if (arrSum /= (2*me*msize-msize+1)*msize/2) then
        print *, 'test 1: arrSum failed at image', me
        error stop 1
    end if

    if (arrSumRight /= (2*right*msize-msize+1)*msize/2) then
        print *, 'test 1: arrSumRight failed at image', me
        error stop 1
    end if

    sync all
end
