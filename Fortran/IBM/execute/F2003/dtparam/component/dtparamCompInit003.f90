!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/26/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.3.4: default init)
!                               Case: Use of kind type parameters in the default
!                               initializations for array component.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (n)
        integer, len :: n

        integer :: id (n)
    end type

    type collector (num)
        integer, kind :: num = 10

        type(base(num)) :: b1(num) = (/(base(num)((/(i, i=j,j+num-1)/)), j=1,num)/)
    end type
end module

program dtparamCompInit003
use m
    type (collector) co1
    class(collector(20)), allocatable :: co2

    allocate(co2)

    do i = 1, 10
        if (any (co1%b1(i)%id /= (/(j, j=i, i+9)/))) error stop 1_4
    end do

    do i = 1, 20
        do j = 1, 20
            if (co2%b1(i)%id(j) /= i+j-1) error stop 2_4
        end do
    end do
end
