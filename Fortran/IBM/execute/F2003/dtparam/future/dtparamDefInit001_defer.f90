!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/22/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.9: structure constr)
!                               Case: Structure constructor for derived type
!                               with default initializations.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k, n)
        integer, kind :: k, n

        real(k) :: data(n) = (/(j*1.0d0, j=0,n-1)/)
    end type

    type, extends(base) :: child
        logical(k) :: flag(n) = (/(mod(j, 2)==0, j=1, n)/)
    end type

    class(base(4, 22)), allocatable :: b1(:)
end module

program dtparamDefInit001
use m
    type(child(8, 33)) :: c1

    logical, external :: precision_r4, precision_r8

    allocate (b1(10), source=base(4, 22)())

    c1 = child(8, 33)()

    do i = 1, 10
        do j = 1, b1%n
            if (.not. precision_r4(b1(i)%data(j), (j-1)*1.0e0)) error stop 1_4
        end do
    end do

    do i = 1, c1%n
        if (.not. precision_r8(c1%data(i), (i-1)*1.0d0)) error stop 2_4

        if (mod(i, 2) == 0) then
            if (.not. c1%flag(i)) error stop 3_4
        else
            if (c1%flag(i)) error stop 4_4
        end if
    end do
end
