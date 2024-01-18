! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/17/2006
!*
!*  DESCRIPTION                : dtparam (section 4.8: array constructor)
!                               Case: Use the type parameter inquiry for
!                               dummy-arg for data assignments.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k, n)
        integer, kind :: k
        integer, len :: n

        integer(k) :: id(n) = -1
    end type

    contains

    subroutine assignVal8 (b, start)
        class(base(8,*)), intent(out) :: b
        integer(8), intent(in) :: start

        b%id = (/(i, i=start, start+b%n-1)/)
    end subroutine
end module

program dtparamArrConstr004a
use m
    class(base(8,:)), allocatable :: b1(:)

    allocate (base(8,20) :: b1(10))

    do i = 1, 10
        call assignVal8 (b1(i), i*100_8)
    end do

    !! verify results
    do i = 1, 10
        if (any(b1(i)%id /= (/(i*100+j, j=0,19)/))) error stop 1_4
    end do
end
