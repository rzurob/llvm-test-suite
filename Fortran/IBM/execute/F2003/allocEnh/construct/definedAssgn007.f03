! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/27/2006
!*
!*  DESCRIPTION                : allocatable enhancement
!                               Test that the defined assignment is
!                               invoked at extended type level.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type dataType
        real, allocatable :: data(:)

        contains

        procedure :: assgnSorted
        generic :: assignment(=) => assgnSorted

        procedure :: sort
    end type

    type base
        integer id
    end type

    type, extends(base) :: child
        type(dataType), allocatable :: data

        contains

        procedure :: assgnC1C2
        generic :: assignment(=) => assgnC1C2
    end type

    contains

    subroutine assgnC1C2 (c1, c2)
        class(child), intent(out) :: c1
        class(child), intent(in) :: c2

        print *, 'if you get in here, it is wrong'
        stop 10
    end subroutine

    subroutine assgnSorted (c1, c2)
        class(dataType), intent(inout) :: c1
        type(dataType), intent(in) :: c2

        c1%data = c2%data
        c1%data = c1%sort()
    end subroutine

    !! function that returns an allocatable array of b%data sorted ascedingly
    real function sort (b)
        class(dataType), intent(in) :: b

        allocatable :: sort(:)

        if (.not. allocated(b%data)) then
            sort = [real :: ]
        else
            sort = b%data

            do i = lbound(sort,1), ubound(sort,1)
                do j = lbound(sort,1), i
                    if (sort(j) > sort(i)) then
                        temp = sort(j)

                        sort(j) = sort(i)
                        sort(i) = temp
                    end if
                end do
            end do
        end if
    end function
end module

module m1
use m
    type container
        class(base), allocatable :: data
    end type
end module

program definedAssgn007
use m1
    type (container), allocatable :: co1, co3(:)
    type(container) co2, co4(0:3)

    real rArray(0:199)
    real, allocatable :: rArraySorted(:), temp

    logical(4), external :: precision_r4

    rArray = [(i*1.0, i=50,1,-1), (log(i+1.0), i=1,100), &
        (sqrt(i*1.0), i=50,1,-1)]

    rArraySorted = rArray

    !! sort out rArray descedently and store in rArraySorted
    do i = lbound(rArraySorted,1), ubound(rArraySorted,1)
        do j = lbound(rArraySorted,1), i
            if (rArraySorted(i) > rArraySorted(j)) then
                temp = rArraySorted(i)

                rArraySorted(i) = rArraySorted(j)
                rArraySorted(j) = temp
            end if
        end do
    end do

    !! test1: scalar case
    allocate (co2%data, source=child(10, dataType(rArray)))

    co1 = co2


    select type (x => co1%data)
        type is (child)
            if (x%id /= 10) error stop 2_4

            do i = 0, 199
                if (.not. precision_r4(x%data%data(i), rArraySorted(199-i))) &
                        error stop 3_4
            end do

        class default
            error stop 1_4
    end select


    !! test2: array case
    allocate(co4(0)%data, source=base(20))
    allocate(co4(1)%data, source=child(30, dataType(rArray(0:49))))
    allocate(co4(2)%data, source=child(40, dataType(rArray(150:))))
    allocate(co4(3)%data, source=child(50, dataType(rArray(50:149))))

    co3 = co4

    !! verify co3
    if ((lbound(co3,1) /= 0) .or. (ubound(co3,1) /= 3)) error stop 5_4

    if (.not. same_type_as(co3(0)%data, base(-100))) error stop 6_4

    if ((co3(0)%data%id /= 20) .or. (co3(1)%data%id /= 30) .or. &
        (co3(2)%data%id /= 40) .or. (co3(3)%data%id /= 50)) error stop 7_4

    select type (x => co3(1)%data)
        type is (child)
            if (size(x%data%data) /= 50) error stop 28_4

            do i = 1, 50
                if (.not. precision_r4(x%data%data(i), i*1.0)) error stop 8_4
            end do

        class default
            error stop 9_4
    end select


    select type (x => co3(2)%data)
        class is (child)
            if (size(x%data%data) /= 50) error stop 29_4

            do i = 1, 50
                if (.not. precision_r4(x%data%data(i), sqrt(i*1.0))) &
                    error stop 10_4
            end do

        class default
            error stop 11_4
    end select

    select type (x => co3(3)%data)
        type is (child)
            if (size(x%data%data) /= 100) error stop 30_4

            do i = 1, 100
                if (.not. precision_r4(x%data%data(i), log(i+1.0))) &
                        error stop 12_4
            end do

        class default
            error stop 13_4
    end select
end