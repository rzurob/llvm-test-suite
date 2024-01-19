! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/08/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.8: derived-type-spec)
!                               Case: A test on parameter keyword.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type A (k, n)
        integer, kind :: k
        integer, len :: n

        real(k) :: data(n) = 1.0
    end type

    type base (ka, n, l)
        integer, kind :: ka = 4
        integer, len :: n = 10, l = 20

        character(len=l) :: name
        type(A(n=n, k=ka)) :: a1! = A(n=n,k=ka)(1.0)
        class(A(n=:, k=4)), allocatable :: data(:)
    end type
end module

program dtparamKeyword001
use m
    type (base(l=:, n=:, ka=8)), allocatable :: b1(:)

    logical(4), external :: precision_r4, precision_r8

    allocate (base(l=20, n=10, ka=8):: b1(2))

    do i = 1, 10
        if (.not. precision_r8 (b1(1)%a1%data(i), 1.0d0)) error stop 10_4
        if (.not. precision_r8 (b1(2)%a1%data(i), 1.0d0)) error stop 11_4
    end do

    !! set up values for b1(1)
    b1(1)%name = 'xlftest test cases for dtparameter'
    b1(1)%a1%data = (/(i*1.0d0, i=1,10)/)

    allocate (A(n=30, k=4) :: b1(1)%data(-2:2))

    do i = -2, 2
        b1(1)%data(i)%data = (/((i+3)*1.0e2+j, j=1,30)/)
    end do

    !! test the intrinsic assignment
    b1(2) = b1(1)

    !! verify b1(2)
    if (b1(2)%name /= 'xlftest test cases f') error stop 1_4

    do i = 1, 10
        if (.not. precision_r8 (b1(2)%a1%data(i), i*1.0d0)) error stop 2_4
    end do

    if (.not. allocated(b1(2)%data)) error stop 3_4

    do i = 1, 5
        do j = 1, 30
            if (.not. precision_r4(b1(2)%data(i-3)%data(j), i*1.0e2+j)) &
                    error stop 4_4
        end do
    end do
end
