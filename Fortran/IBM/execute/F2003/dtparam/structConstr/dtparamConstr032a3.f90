!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/14/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.9: structure constructor)
!                               Case: a variant of dtparamConstr032a2.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k, n)
        integer, kind :: k
        integer, len :: n

        complex(k) :: cx(n)
    end type

    type container (k, n, dim1, dim2)
        integer, kind :: k
        integer, len :: n, dim1, dim2

        type(base(k,n)) :: data(dim1, dim2)
    end type

    type(container(4, 50, 20, 60)), parameter :: co_const = &
        container(4, 50, 20, 60)(base(4,50)(cx=(/(cmplx(i), i=1,50)/)))
end module

program dtparamConstr032a3
use m
    type (container(8,:,:,:)), allocatable :: co1

    logical(4), external :: precision_x8

    integer n, dim1, dim2

    allocate (container(8,70, 45,201) :: co1)

    co1 = container(8,70, 45,201)(base(8,70)(cx=(/co_const%data(1,1)%cx, &
        co_const%data(20,60)%cx(31:50)/)))


    !! verify co1
    do dim1 = 1, 45
        do n = 1, 50
            do dim2 = 1, 201
                if (.not. precision_x8(cmplx(co1%data(dim1,dim2)%cx(n), kind=4), &
                    cmplx(n*1.0, kind=4))) error stop 1_4
            end do
        end do
    end do

    do dim2 = 201, 1, -1
        do n = 51, 70
            do dim1 = 45, 1, -1
                if (.not. precision_x8(cmplx(co1%data(dim1,dim2)%cx(n), kind=4), &
                    cmplx((n-20), kind=4))) error stop 2_4
            end do
        end do
    end do
end
