!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/02/2006
!*
!*  DESCRIPTION                : allocatable enhancement
!                               modified version of intrinsic001
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer, allocatable :: id
    end type

    type, extends(base) :: child
        character(:), allocatable :: name
    end type
end module

program intrinsic001a
use m
    type (base), allocatable :: b1(:,:)
    type (base), pointer :: bptr(:,:)
    type (base), target :: b2 (1000)

    type(child), allocatable :: c1(:,:)
    type(child), pointer :: cptr(:,:)
    type(child), target :: c2(800)

    allocate (b1(0:1,0:9), c1(0:19, 20))

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

    !! test3: test the child type
    do i = 1, 800
        c2(i)%id = 1000+i
        c2(i)%name = repeat(achar(mod(i,128)+1), i/2)
    end do

    cptr(-1:18, 0:19) => c2(2::2)

    !! the next assignment will NOT cause c1 to be reallocated since there is no
    !change in shape
    c1 = cptr

    if (any(lbound(c1) /= [0,1]) .or. any(ubound(c1) /= [19,20])) error stop 13_4

    k = 2

    do j = 1, 20
        do i = 0, 19
            if (c1(i,j)%id /= 1000+k) error stop 4_4
            if (c1(i,j)%name /= repeat(achar(mod(k,128)+1), k/2)) error stop 5_4

            k = k + 2
        end do
    end do

    !! next assignment will casue c1 to be reallocated
    c1 = transpose (c1(:,::2))

    if (any(lbound(c1) /= 1) .or. any(ubound(c1) /= [10,20])) error stop 14_4

    k = 2

    do i = 1, 10
        do j = 1, 20
            if (c1(i,j)%id /= 1000+k) error stop 6_4
            if (c1(i,j)%name /= repeat(achar(mod(k,128)+1), k/2)) error stop 7_4

            k = k + 2
        end do

        k = k + 40
    end do
end
