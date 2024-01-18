!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/29/2006
!*
!*  DESCRIPTION                : allocatable enhancement
!                               Test the intrinsic function results as the expr
!                               used in the intrinsic assignment; test
!                               transpose; also use type bound elemental defined
!                               operator.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer, allocatable :: id

        contains

        procedure :: addB1B2
        generic :: operator(+) => addB1B2
    end type

    contains

    elemental function addB1B2 (b1, b2)
        class(base), intent(in) :: b1
        type(base), intent(in) :: b2

        type(base) addB1B2

        if (allocated(b1%id)) then
            if (allocated(b2%id)) then
                addB1B2%id = b1%id + b2%id
            else
                addB1B2%id = b1%id
            end if
        else
            if (allocated(b2%id)) addB1B2%id = b2%id
        end if
    end function
end module

program intrinsic001
use m
    type (base), allocatable :: b1(:,:)
    type (base), pointer :: bptr(:,:)
    type (base), target :: b2 (1000)

    allocate (b1(0:1,0:9))

    b2 = [(base(i), i=1,1000)]

    do i = 0, 1
        do j = 0, 9, 2
            b1(i,j)%id = i+j
        end do
    end do

    !! first test: transpose b1 itself
    b1 = transpose(b1)

    if (any(lbound(b1) /= 1) .or. any(ubound(b1) /= [10,2])) error stop 11_4

    k = 0
    do i = 1, 10, 2
        do j = 1, 2
            if ((.not. allocated(b1(i,j)%id)) .or. &
                allocated(b1(i+1,j)%id)) error stop 1_4

            if (b1(i,j)%id /= k) error stop 2_4

            k = k + 1
        end do
    end do

    !! test 2: bptr => b2 and assigned to b1 via transpose
    bptr (0:20, 0:19) => b2(::2)

    b1 = transpose (bptr)

    if (any(lbound(b1) /= 1) .or. any(ubound(b1) /= [20,21])) error stop 12_4

    k = 1

    do i = 1, 20
        do j = 1, 21
            if (b1(i,j)%id /= k) error stop 3_4

            k = k + 2
        end do
    end do

    !! test 3: invoke the defined type-bound operator + in calling transpose
    b1 = transpose (b1 + b1)

    if (any(lbound(b1) /= 1) .or. any(ubound(b1) /= [21,20])) error stop 13_4

    k = 1

    do j = 1, 20
        do i = 1, 21
            if (b1(i,j)%id /= 2*k) error stop 4_4

            k = k + 2
        end do
    end do
end
