! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/18/2006
!*
!*  DESCRIPTION                : allocatable enhancement
!                               Test that the derived type dummy allocatable
!                               argument used in the intrinsic assignment.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        real(8), allocatable :: data(:)
        logical, pointer :: mask(:)

        contains

        procedure :: setMask => setBaseMask
        procedure :: reduce => reduceBaseData
    end type

    contains

    subroutine copyBaseArray (b1, b2)
        type(base), allocatable :: b1(:), b2(:)

        b1 = b2
    end subroutine

    subroutine setBaseMask (b, d1, cmp)
        class(base) b
        double precision d1
        procedure(logical) cmp

        if (allocated(b%data)) then
            allocate(b%mask(size(b%data)))

            b%mask = [(cmp(b%data(i), d1),&
                i=lbound(b%data,1), ubound(b%data,1))]
        else
            nullify(b%mask)
        end if
    end subroutine

    subroutine reduceBaseData (b)
        class(base) b

        if (associated(b%mask)) then
            b%data = pack(b%data, b%mask)

            deallocate(b%mask)
        else
            stop 10
        end if
    end subroutine
end module

module m1
use m, only: base
    type dataType
        double precision, allocatable :: data(:)
    end type
    contains

    subroutine moveData (d1, b1)
        type(base), allocatable :: b1(:)
        class(dataType) d1(:)

        allocate (b1(size(d1)))

        do i = 1, size(b1)
            call move_alloc (d1(i)%data, b1(i)%data)
        end do
    end subroutine

    subroutine setMask (b1, d1, func)
        type(base), allocatable :: b1(:)
        real(8) d1(size(b1))
        procedure(logical) func

        do i = 1, size(b1)
            call b1(i)%setMask(d1(i), func)
        end do
    end subroutine
end module


program dummyArg007
use m1
use m, only: copyBaseArray
    type(base), allocatable :: b1(:), b2(:)

    type(dataType), allocatable :: d1(:)

    logical, external :: less, precision_r8

    d1 = [(dataType([(j+1, j=0,i*2)]), i=1,10)]

    call moveData (d1, b1)

    if (size(b1) /= 10) error stop 1_4

    call setMask (b1, [(i*1.0d0, i=2,11)], less)

    do i = 1, 10
        call b1(i)%reduce
    end do

    call copyBaseArray (b2, b1)

    !! verify d1, b1 and b2
    do i = 1, 10
        if (allocated(d1(i)%data)) error stop 2_4

        if (associated(b1(i)%mask) .or. associated(b2(i)%mask)) error stop 3_4

        if ((size(b1(i)%data) /= i) .or. (size(b2(i)%data) /= i)) error stop 4_4

        do j = 1, i
            if (.not. precision_r8 (b1(i)%data(j), j*1.0d0)) error stop 5_4
            if (.not. precision_r8 (b2(i)%data(j), j*1.0d0)) error stop 6_4
        end do
    end do
end

logical function less (d1, d2)
    double precision d1, d2
    less = d1 < d2
end function
