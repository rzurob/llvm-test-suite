! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/28/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.9: structure constr)
!                               Case: Structure constructor for components that
!                               are of derived type in the struture constructor.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k, n)
        integer, kind :: k
        integer, len :: n

        real(k) :: data(n)
        integer(k) :: id(n)
    end type

    type container (k, n)
        integer, kind :: k
        integer, len :: n

        type(base(k, n)) :: data
    end type

    type(container(8, 22)) :: co1 = container(8,22)&
        (base(8,22)(1.2d0, (/(2_8**30*i, i=1,22)/)))
end module

program dtparamConstr009a
use m
    type(container(4, 21)) :: co2
    type(container(8, :)), allocatable :: co3(:)

    logical(4), external :: precision_r4, precision_r8

    co2 = container(4, 21)(data=base(4,21)((/(i*1.0, i=1,21)/), &
            id=(/(i, i=21, 1, -1)/)))


    allocate(co3(10), source=(/(container(8,33)(base(8,33) &
            (id=i, data=i*1.1d0)), i=1,10)/))

    !! verify results
    do i = 1, 22
        if (.not. precision_r8(co1%data%data(i), 1.2d0)) error stop 1_4

        if (co1%data%id(i) /2**30 /= i) error stop 2_4
    end do

    do i = 1, 21
        if (.not. precision_r4(co2%data%data(i), i*1.0)) error stop 3_4

        if (co2%data%id(i) /= 22-i) error stop 4_4
    end do

    do i = 1, 10
        do j = 1, 33
            if (.not. precision_r8(co3(i)%data%data(j), i*1.1d0)) error stop 5_4

            if (co3(i)%data%id(j) /= i) error stop 6_4
        end do
    end do
end
