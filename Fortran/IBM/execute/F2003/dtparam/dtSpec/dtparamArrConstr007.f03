! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/21/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.8: derived-type-spec)
!                               Case: Use the dummy-arg with deferred parameter
!                               for allocation; use type-spec in array
!                               constructor; length type parameter is variable.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (n)
        integer, len :: n

        integer :: id(n) = -1
    end type

    contains

    subroutine setID (b, i1)
        class(base(:)), allocatable, intent(out) :: b
        integer, intent(in) :: i1(:)

        allocate (base(size(i1)) :: b)
        b%id = i1
    end subroutine
end module

program dtparamArrConstr007
use m
    class(base(:)), allocatable :: b1, b2

    type(base(:)), allocatable :: b3(:)

    integer, allocatable :: i1(:)

    allocate (base(15) :: b1)

    allocate (i1(10), source=(/(j, j=1,10)/))

    call setID (b1, i1)
    call setID (b2, i1+10)

    allocate (base(size(i1)) :: b3(0:1))

    b3 = (/base(size(i1)) :: b1, b2/)

    !! verify
    if (any(b3(0)%id /= (/(j, j=1, 10)/)) .or. &
        any(b3(1)%id /= (/(j, j=11, 20)/))) error stop 1_4
end
