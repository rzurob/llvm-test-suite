!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/26/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.3.4: default init)
!                               Case: Default initializations of parameterized
!                               component with array constructor; but without
!                               AC-IMPDO.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (n)
        integer, len :: n

        integer :: id (n)
    end type

    type collector
        type(base(10)) :: b1(10) = [ base(10)([(i, i=1,10)]), &
            base(10)([(i, i=2,11)]), base(10)([(i, i=3,12)]), &
            base(10)([(i, i=4,13)]), base(10)([(i, i=5,14)]), &
            base(10)([(i, i=6,15)]), base(10)([(i, i=7,16)]), &
            base(10)([(i, i=8,17)]), base(10)([(i, i=9,18)]), &
            base(10)([(i, i=10,19)]) ]
    end type
end module

program dtparamCompInit001
use m
    type (collector) co1
    class(collector), allocatable :: co2

    allocate(co2)

    do i = 1, 10
        if (any (co1%b1(i)%id /= (/(j, j=i, i+9)/))) error stop 1_4
        if (any (co2%b1(i)%id /= (/(j, j=i, i+9)/))) error stop 2_4
    end do
end
