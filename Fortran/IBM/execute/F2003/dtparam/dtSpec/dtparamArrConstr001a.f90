!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/16/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.8: derived-type-spec)
!                               Case: Test type-spec in array constructor; take
!                               rank-two array to rank one.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k, n)
        integer, kind :: k
        integer, len :: n

        real(k) :: data(n)
    end type
end module

program dtparamArrConstr001
use m
    type (base(8, 23)) b1(10)

    type (base(8, :)), pointer :: b2(:,:)

    logical(4), external :: precision_r8

    integer :: indices (10)

    allocate (base(8, 23) :: b2(2,5))

    do j = 1, 5
        do i = 1, 2
            b2(i, j)%data = (/(i*1.0d3+j*1.0d2+k, k = 1, 23)/)
        end do
    end do

    indices = (/integer:: 11, 21, 12, 22, 13, 23, 14, 24, 15, 25/)

    b1 = (/b2/)

    do i = 1, 10
        do j = 1, 23
            if (.not. precision_r8(indices(i)*1.0d2+j, b1(i)%data(j))) &
                    error stop 1_4
        end do
    end do
end
