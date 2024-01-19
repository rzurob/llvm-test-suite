! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/13/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.9: structure constructor)
!                               Case: Use of scalars for multi-dimensional array
!                               components.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module mod
    type base (k, dim1, dim2)
        integer, kind :: k
        integer, len :: dim1, dim2

        real(k) :: data(dim1, dim2)
    end type

    type container (k, dim1, dim2, n, m)
        integer, kind :: k
        integer, len :: dim1, dim2, n, m

        type (base(k,dim1,dim2)) :: data(n,m)
    end type

    type (container(8,:,:,:,:)), allocatable :: co1
end module

program dtparamConstr032a2
use mod
    type (container(4,55,102, 10,45)) :: co2 = &
        container(4,55,102, 10,45)(base(4,55,102)(-1))

    logical(4), external :: precision_r4, precision_r8

    complex(8), parameter :: c1 = (1.2d0, 2.0d0)

    type (container(4,31,21, 105,23)) :: co3

    integer dim1, dim2, n, m

    allocate (container(8,22,83, 43,99) :: co1)

    co1 = container(8,22,83, 43,99)(data=base(8,22,83)(data = c1))

    co3 = container(4,31,21, 105,23)(data = base(4,31,21)(square(2.5)))


    !! verify co1, co2 and co3

    do m = 1, 99
        do n = 1, 43
            do dim2 = 1, 83
                do dim1 = 1, 22
                    if (.not. precision_r8(co1%data(n,m)%data(dim1, dim2), &
                        1.2d0)) error stop 1_4
                end do
            end do
        end do
    end do

    do dim1 = 1, 55
        do dim2 = 1, 102
            do n = 1, 10
                do m = 1, 45
                    if (.not. precision_r4(co2%data(n,m)%data(dim1, dim2), &
                        -1.0)) error stop 2_4
                end do
            end do
        end do
    end do

    do dim2 = 1, 21
        do dim1 = 1, 31
            do m = 1, 23
                do n = 1, 105
                    if (.not. precision_r4(co3%data(n,m)%data(dim1, dim2), &
                        6.0)) error stop 3_4
                end do
            end do
        end do
    end do


    contains

    integer function square (x)
        square = x * x
    end function
end
