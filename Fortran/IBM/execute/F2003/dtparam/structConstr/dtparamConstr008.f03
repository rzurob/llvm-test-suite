! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/23/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.9: structure constr)
!                               Case: Expression used in the component data
!                               source; use parameterized derived type.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k, n)
        integer, kind :: k
        integer, len :: n

        real(k) :: data(n)
    end type

    type container(k,n, m)
        integer, kind :: k
        integer, len :: n, m

        type(base(k, n)) :: data(m)
    end type
end module

program dtparamConstr008
use m
    type(container(8, 20, 10)) :: co1

    logical(4), external :: precision_r8

    co1 = container(8,20,10)((/(base(8,20)((/(log(j*1.0d2+i), i=1,20)/)),&
            j=1,10)/))

    !! verify
    do i = 1, 10
        do j = 1, 20
            if (.not. precision_r8(co1%data(i)%data(j), log(i*1.0d2+j))) &
                    error stop 1_4
        end do
    end do
end
