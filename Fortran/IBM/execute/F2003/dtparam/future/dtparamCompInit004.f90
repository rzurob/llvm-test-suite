!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/26/2006
!*
!*  DESCRIPTION                :dtparam (section 4.5.3.4: default init.)
!                               Case: Use the named constant for default
!                               initialization.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k)
        integer, kind :: k

        integer(k) :: id
    end type

    type(base(4)), parameter :: b1_const = base(4)(id = 1)
    type(base(8)), parameter :: b2_const(10) = (/(base(8)(-i), i=1, 10)/)

    type container (size)
        integer, kind :: size

        type(base(4)) :: b1(size) = b1_const

        type(base(8)) :: b2(10) = b2_const
    end type
end module

program dtparamCompInit004
use m
    type (container(20)), pointer :: co1(:)
    class(container(30)), allocatable :: co2(:)

    allocate (co1(10), co2(3))

    do i = 1, 20
        if (co1(8)%b1(i)%id  /= 1) error stop 1_4

        if (i < 11) then
            if (co1(7)%b2(i)%id /= -i) error stop 2_4
        end if
    end do

    do i = 1, 30
        if (co2(1)%b1(i)%id /= 1) error stop 3_4

        if (i < 11) then
            if (co2(2)%b2(i)%id /= -i) error stop 4_4
        end if
    end do

    deallocate (co1, co2)
end
