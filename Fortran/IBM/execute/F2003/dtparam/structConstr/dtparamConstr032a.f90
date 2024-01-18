!#######################################################################
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

module m
    type base (k, dim1, dim2, dim3)
        integer, kind :: k
        integer, len :: dim1, dim2, dim3

        complex(k) :: cx(dim1, dim2, dim3)
    end type
end module

program dtparamConstr032a
use m
    type (base(8,:,:,:)), allocatable :: b1
    type(base(4,:,:,:)), pointer :: b2(:)

    logical(4), external :: precision_x8

    allocate (base(8, 10, 12, 32) :: b1)

    b1 = base(8, 10, 12, 32)((2.1**2, 1.2**3))

    allocate (b2(10), source=base(4, 23, 45, 71)(3.2*1.2))

    !! verify
    do k = 1, 32
        do j = 1, 12
            do i = 1, 10
                if (.not. precision_x8(cmplx(b1%cx(i,j,k)),(2.1**2,1.2**3)))&
                    error stop 1_4
            end do
        end do
    end do

    do icount = 1, 10
        do k = 1, 71
            do j = 1, 45
                do i = 1, 23
                    if (.not. precision_x8(b2(icount)%cx(i,j,k), &
                        (3.2*1.2, 0.0e0))) error stop 2_4
                end do
            end do
        end do
    end do
end
