! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/23/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.9: structure constr)
!                               Case: Data target in the structure constructor;
!                               derived type with type parameters.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k, n)
        integer, kind :: k
        integer, len :: n

        real(k) :: data(n)
    end type

    type container (k)
        integer, kind :: k

        type(base(k, :)), pointer :: data(:)
    end type
end module

program dtparamConstr006
use m
    type (base(8, 33)), target :: b1(0:9)
    class(base(4,:)), pointer :: b2(:)

    type (container(8)) :: co1
    type (container(4)) :: co2

    logical(4), external :: precision_r4, precision_r8

    nullify (b2)

    do i = 1, 10
        b1(i-1)%data = (/(i*1.0d2+j, j=1, 33)/)
    end do

    co1 = container(8)(b1)
    co2 = container(4)(b2)

    if (associated(co2%data) .or. (.not. associated(co1%data, b1))) &
            error stop 1_4


    if (co1%data%n /= 33) error stop 2_4

    if ((lbound(co1%data, 1) /= 0) .or. (ubound(co1%data, 1) /= 9)) &
            error stop 3_4

    do i = 0, 9
        do j = 1, 33
            if (.not. precision_r8(co1%data(i)%data(j), (i+1)*1.0d2+j)) &
                    error stop 4_4
        end do
    end do
end
