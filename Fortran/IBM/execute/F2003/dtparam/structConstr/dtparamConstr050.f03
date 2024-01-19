! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 08/11/2006
!*
!*  DESCRIPTION                : derived type parameter
!                               Test the allocatable component of a derived type
!                               with type parameters; test the allocation status
!                               using an allocatable object as the data-source;
!                               test derived type as components.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type realArray (k)
        integer, kind :: k

        real(k), allocatable :: data(:)
    end type

    type string (n)
        integer, len :: n

        character(n) :: name
    end type

    type base (k, n)
        integer, kind :: k
        integer, len :: n

        type(realArray(k)), allocatable :: data
        class(string(n)), allocatable :: name
    end type

    contains

    subroutine printBase4 (b)
        type (base(4,*)), intent(in) :: b

        if ((.not. allocated(b%data)) .or. (.not. allocated(b%name))) error stop 10

        if (.not. allocated(b%data%data)) error stop 11

        do i = lbound(b%data%data,1), ubound(b%data%data,1)
            write (*, '(dc, i4, 1x, e12.3)', advance='no') i, b%data%data(i)
        end do

        write (*,*) b%name%name
    end subroutine
end module

program dtparamConstr050
use m
    type(base(4, 10)) b1

    real :: r1(0:9)

    logical(4), external :: precision_r4

    r1 = (/(i, i=1,10)/)

    b1 = base(4,10)(realArray(4)(r1), name=string(10)(name='xlftest 101'))

    if ((.not. allocated(b1%data)) .or. (.not. allocated(b1%name))) &
            error stop 1_4

    if (.not. allocated(b1%data%data)) error stop 2_4

    if ((lbound(b1%data%data,1) /= 0) .or. (ubound(b1%data%data,1) /= 9)) &
            error stop 3_4

    do i = 0, 9
        if (.not. precision_r4(b1%data%data(i), (i+1)*1.0)) error stop 4_4
    end do

    if (b1%name%n /= 10) error stop 5_4

    if (b1%name%name /= 'xlftest 10') error stop 6_4

    call printBase4(b1)
end
