!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/09/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.8: derived-type-spec)
!                               Case: Value conversion from integer(4) to
!                               integer(1).
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k, n)
        integer(1), kind :: k
        integer(1), len :: n

        real(k) :: data(n)
    end type
end module

program dtparamValConvert001
use m
    type (base(4, n= 20)) :: b1(10)
    class(base(8, n=:)), allocatable :: b2

    logical(4), external :: precision_r4, precision_r8

    allocate (base(8,100) :: b2)

    do i = 1, 10
        b1(i)%data = (/(i*1.0e2+j, j = 1, 20)/)
    end do

    b2%data = sinh((/(i*1.0d0, i=1,100)/))

    !! verify
    do i = 1, 10
        do j = 1, 20
            if (.not. precision_r4 (b1(i)%data(j), i*1.0e2+j)) error stop 1_4
        end do
    end do

    do i = 1, 100
        if (.not. precision_r8(b2%data(i), sinh(i*1.0d0))) error stop 2_4
    end do
end
