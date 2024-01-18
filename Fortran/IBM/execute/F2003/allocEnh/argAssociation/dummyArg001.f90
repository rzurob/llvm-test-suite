! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/13/2006
!*
!*  DESCRIPTION                : allocatable enhancement
!                               Use the dummy arg as the expr for the intrinsic
!                               assignment; use explicit-shape array for the
!                               dummy-arg and test bounds after assignment.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    real, allocatable :: r1(:,:)

    type base
        integer :: id
        character(:), allocatable :: name
    end type

    type, extends (base) :: child
        logical, allocatable :: flag
    end type

    type(child), allocatable :: c1(:)
end module

program dummyArg001
use m
    real r2(100)

    class(base), pointer :: b1(:,:)

    logical(4), external :: precision_r4

    r2 = (/(i, i=1,100)/)

    !! test1: for intrinsic type
    call setR1 (r2(::2), 0,9, -1,3)

    if ((.not. allocated(r1)) .or. (size(r1) /= 50)) error stop 1_4

    if (any(lbound(r1) /= (/0, -1/)) .or. any(ubound(r1) /= (/9, 3/))) &
            error stop 2_4

    k = 1

    do j = -1, 3
        do i = 0, 9
            if (.not. precision_r4(k*1.0, r1(i,j))) error stop 3_4

            k = k + 2
        end do
    end do

    call setR1 ((/(i*1.0, i=1,100)/), 1,5, 0,9)

    if ((.not. allocated(r1)) .or. (size(r1) /= 50)) error stop 4_4

    if (any(lbound(r1) /= (/1, 0/)) .or. any(ubound(r1) /= (/5, 9/))) &
            error stop 5_4


    k = 1

    do j = 0, 9
        do i = 1, 5
            if (.not. precision_r4(k*1.0, r1(i,j))) error stop 6_4

            k = k + 1
        end do
    end do

    !! the next call will not change the bounds of r1
    call setR1 ((/(i*1.0, i=100,1,-1)/), 0,4, -2,7)

    if (any(lbound(r1) /= (/1, 0/)) .or. any(ubound(r1) /= (/5, 9/))) &
            error stop 25_4


    k = 100

    do j = 0, 9
        do i = 1, 5
            if (.not. precision_r4(k*1.0, r1(i,j))) error stop 26_4

            k = k - 1
        end do
    end do


    !! test2: derived type
    allocate (child :: b1(10,10))

    select type (b1)
        type is (child)
            b1%id = reshape((/(i, i=1,100)/), (/10,10/))

            do i = 1, 10
                do j = 1, 10
                    b1(i,j)%name = 'xlftest ' //achar(64+i+j)
                    b1(i,j)%flag = mod(i+j,2) == 0
                end do
            end do

            call setC1 (b1, -1, 70)

        class default
            stop 20
    end select

    if (.not. allocated(c1)) error stop 8_4

    if ((lbound(c1,1) /= -1) .or. (ubound(c1, 1) /= 70)) error stop 9_4

    k = 1

    do i = -1, 70
        if (c1(i)%id /=  k) error stop 10_4

        if ((.not. allocated(c1(i)%name)) .or. &
            (.not. allocated(c1(i)%flag))) error stop 11_4

        if (mod(k,10) == 0) then
            if (c1(i)%name /= 'xlftest ' // achar(74+k/10)) error stop 12_4

            if (c1(i)%flag .neqv. (mod(k/10,2) == 0)) error stop 13_4
        else
            if (c1(i)%name /= 'xlftest '//achar(65+mod(k,10)+k/10)) &
                    error stop 14_4

            if (c1(i)%flag .neqv. (mod(mod(k,10)+k/10+1, 2) == 0)) &
                    error stop 15_4
        end if

        k = k + 1
    end do
end


subroutine setR1 (r2, lb1,ub1, lb2, ub2)
use m, only: r1
    integer, intent(in) :: lb1, ub1, lb2, ub2
    real, intent(in) :: r2(lb1:ub1, lb2:ub2)

    r1 = r2
end subroutine

subroutine setC1 (c2, lb, ub)
use m, only: child, c1
    type(child), intent(in) :: c2(lb:ub)
    integer, intent(in) :: lb, ub

    c1 = c2
end subroutine
