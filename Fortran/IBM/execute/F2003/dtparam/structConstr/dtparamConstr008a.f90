!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/24/2006
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

program dtparamConstr008a
use m
    type(container(8, 20, 10)) :: co1
    type(container(4, :,:)), pointer :: co2

    logical(4), external :: precision_r8, precision_r4

    co1 = container(8,20,10)(base(8,20)(log(4.5d1)))

    allocate (co2, source=container(4, 33, 21)(base(4,33)(1.22)))

    !! verify
    do i = 1, 10
        do j = 1, 20
            if (.not. precision_r8(co1%data(i)%data(j), log(4.5d1))) &
                    error stop 1_4
        end do
    end do

    do i = 1, 21
        do j = 1, 33
            if (.not. precision_r4(co2%data(i)%data(j), 1.22)) error stop 2_4
        end do
    end do
end
