! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/12/2006
!*
!*  DESCRIPTION                : dtparam (section 4.2: type parameters)
!                               Case: Deferred type-parameters: defined during
!                               intrinisic assignment; use allocatable entities.
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

program deferdparamDTSpec013
use m
    type (base(8,:)), allocatable :: b1(:)
    type (base(8, 10)) b2(20)
    type (base(8, 30)) b3(15)

    logical(4), external :: precision_r8

    do i = 1, 20
        b2(i)%data = (/(i*1.0d2+j, j=1,10)/)
    end do

    do i = 1, 15
        b3(i)%data = (/(i*1.0d3+j, j=1, 30)/)
    end do

    !! test 1
    b1 = b2

    !! verify b1
    if (.not. allocated(b1)) error stop 1_4

    if ((size(b1) /= 20) .or. (b1%n /= 10)) error stop 2_4

    do i = 1, 20
        do j = 1, 10
            if (.not. precision_r8(i*1.0d2+j, b1(i)%data(j))) error stop 3_4
        end do
    end do


    !! test 2
    b1 = b3

    !! verify b1
    if (.not. allocated(b1)) error stop 4_4

    if ((size(b1) /= 15) .or. (b1%n /= 30)) error stop 5_4

    do i = 1, 15
        do j = 1, 30
            if (.not. precision_r8(b1(i)%data(j), i*1.0d3+j)) error stop 6_4
        end do
    end do
end
