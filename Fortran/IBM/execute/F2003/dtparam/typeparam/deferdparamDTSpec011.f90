!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/12/2006
!*
!*  DESCRIPTION                : dtparam (section 4.2: type parameters)
!                               Case: Deferred type-parameters: defined during
!                               intrinisic assignment; use allocatable
!                               components.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (n)
        integer, len :: n

        real(4) :: data(n)
    end type

    type container
        type (base(:)), allocatable :: data(:)
    end type
end module

program deferdparamDTSpec011
use m
    type (container) co1(100)
    logical(4), external :: precision_r4

    !! set up the values for 1st 20 elements
    do i = 1, 20
        allocate (base(10+i) :: co1(i)%data(i))

        do j = 1, i
            co1(i)%data(j)%data = (/(j*1.0e2+k, k=1,10+i)/)
        end do
    end do

    !! test scalar assgn
    co1(70) = co1(20)

    !! test array assgn
    co1(71:89) = co1(19:1:-1)

    !! verify results
    do i = 70, 89
        do j = 1, 90-i
            do k = 1, 100-i
                if (.not. precision_r4 (co1(i)%data(j)%data(k), j*1.0e2+k)) &
                    error stop 1_4
            end do
        end do
    end do
end
