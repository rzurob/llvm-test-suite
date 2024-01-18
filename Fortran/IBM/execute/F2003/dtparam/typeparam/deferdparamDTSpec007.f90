!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/11/2006
!*
!*  DESCRIPTION                : dtparam (section 4.2: type parameters)
!                               Case: Deferred type-parameters in
!                               declaration-type-spec: function results, use of
!                               module function and poly function return.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (n)
        integer, len :: n

        real :: data(n) = 0.0
        integer :: id = -1
    end type

    contains

    class(base(:)) function genBasePtr (data, id)
        pointer genBasePtr
        real, intent(in) :: data(:)
        integer, intent(in) :: id

        allocate (base(size(data)) :: genBasePtr)

        genBasePtr%data = data
        genBasePtr%id = id
    end function
end module

program deferdparamDTSpec007
use m
    class(base(:)), pointer :: b1, b2, b3

    real, allocatable :: r1(:)

    allocate(r1(-1:10), source=(/(1.e2 + i, i=-1,10)/))

    b1 => genBasePtr ((/(i*1.0, i=1, 20)/), 10)

    b2 => b1

    b1 => genBasePtr (r1, 20)
    b3 => b1

    allocate (base(2*size(r1)):: b1)

    b1%data(1:22) = (/r1, r1(10:1:-1)/)
    b1%id = 30

    !! verrify the results
100 format (i4, (7f10.2))

    write (*, 100) b1%id, b1%data
    write (*, 100) b2%id, b2%data
    write (*, 100) b3%id, b3%data
end
