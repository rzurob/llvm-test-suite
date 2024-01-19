! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/03/2006
!*
!*  DESCRIPTION                : allocatable enhancement
!                               Test the intrinsic results from SPREAD of the
!                               derived type for the intrinsic assignment.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer id
    end type

    type, extends(base) :: child
        character(:), allocatable :: name
    end type
end module

program intrinsic002
use m
    type(base), allocatable :: b1(:,:), b2(:,:,:)

    type(child), allocatable :: c1(:,:), c2(:,:,:), c3(:,:,:,:)

    integer extendsB1

    character(:), pointer :: verifyStr

    allocate (b1(0:9, 0:19))
    allocate (character(20) :: verifyStr)

    do i = 0, 9
        do j = 0, 19
            b1(i,j)%id = i + j
        end do
    end do

    extendsB1 = 5

    !! test1 : spread b1 by dim2 and assign to b2
    b2 = spread(b1, 2, extendsB1)

    do i = 1, 10
        do j = 1, 20
            do k = 1,5
                if (b2(i, k, j)%id /= i+j-2) error stop 1_4
            end do
        end do
    end do


    !! test2: spread c1's first dimension by 5 elements; then spread c2's 3rd
    !dimension by 10 elements
    c1 = reshape([(child(i, 'test '//i2string(i)), i=1,100)], [10,10])

    c2 = spread(c1, 1, 5)

    c3 = spread(c2, 3, 3*extendsB1)

    !! verify c1, c2 and c3

    if (any(lbound(c1) /= 1) .or. any(ubound(c1) /= 10)) error stop 10_4

    if (any(lbound(c2) /= 1) .or. any(ubound(c2) /= [5,10,10])) error stop 11_4

    if (any(lbound(c3) /= 1) .or. any(ubound(c3) /= [5,10,15,10])) &
        error stop 12_4


    k = 1

    do j = 1, 10
        do i = 1, 10
            if (c1(i,j)%id /= k) error stop 2_4

            write (verifyStr, *) 'test', k

            if (c1(i,j)%name /= verifyStr(2:)) error stop 3_4

            do ic2 = 1, 5
                if (c2(ic2,i,j)%id /= k) error stop 4_4

                if (c2(ic2,i,j)%name /= verifyStr(2:)) error stop 5_4

                do ic3 = 1, 15
                    if (c3(ic2, i, ic3, j)%id /= k) error stop 6_4
                    if (c3(ic2, i, ic3, j)%name /= verifyStr(2:)) error stop 7_4
                end do
            end do

            k = k + 1
        end do
    end do

    !! last test: zero-size array
    c2 = spread (c1, 2, -extendsB1)

    if (any(shape(c2) /= [10, 0, 10])) error stop 15_4

    contains

    character(:) function i2string (i)
        allocatable i2string

        integer :: length
        character(:), allocatable :: fmt

        if (i == 0) then
            length = 1
        else
            length = log10(abs(i)*1.0) + 1
        end if

        if (i < 0) length = length + 1

        i2string = repeat(' ', length)

        fmt = repeat(' ', 20)

        write (fmt, *) '(i', length, ')'
        write (i2string, fmt) i
    end function
end
