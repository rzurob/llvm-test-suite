! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/28/2006
!*
!*  DESCRIPTION                : allocatable enhancement
!                               Test a allocatable function result used as an
!                               expr in an intrinsic assignment for allocatable
!                               variable; test the bounds of the allocatable
!                               variable; function result is of derived type
!                               with allocatable component.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        real(16), allocatable :: data
    end type

    contains

    function genBaseAlloc (q1, lb)
        real(16), intent(in) :: q1(:,:)
        integer, intent(in) :: lb

        type(base), allocatable :: genBaseAlloc(:)

        real(16), allocatable :: q2(:)

        allocate(genBaseAlloc(lb:lb+size(q1)-1))

        q2 = pack (q1, .true.)

        do i = lb, lb+size(q1)-1
            genBaseAlloc(i)%data = q2(i-lb+1)
        end do
    end function
end module


program funcRet003
use m
    real(16) :: q1(20, 30)
    type(base), allocatable :: b1(:)

    logical(4), external :: precision_r6

    q1 = reshape([(log(i*1.5q0), i=1,600)], [20, 30])

    !! test 1: auto-allocate b1
    b1 = genBaseAlloc (q1, 20)

    if (.not. allocated(b1)) error stop 1_4

!    if ((lbound(b1,1) /= 1) .or. (ubound(b1, 1) /= 600)) error stop 2_4

    k = 1

    do i = lbound(b1,1), ubound(b1, 1)
        if (.not. precision_r6 (b1(i)%data, log(k*1.5q0))) error stop 3_4

        k = k + 1
    end do

    !! test2: allocate b1 with a shape that matches the function result; so
    !there will be no auto-reallocation
    deallocate (b1)

    allocate (b1(0:299))

    b1 = genBaseAlloc (q1(::2,:), 20)

    if ((lbound(b1,1) /= 0) .or. (ubound(b1,1) /= 299)) error stop 4_4

    do i = 0, 299
        if (.not. precision_r6 (b1(i)%data, log(2*i+1.0q0) + log(1.5q0))) &
            error stop 5_4
    end do
end
