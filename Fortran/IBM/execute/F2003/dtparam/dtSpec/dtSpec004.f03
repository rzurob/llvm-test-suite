! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/31/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.8: derived-type-spec)
!                               Case: Keyword can be omitted only if the
!                               preceding keyword is omitted; use keyword
!                               different ways.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k)
        integer, kind :: k

        real(k) :: data = -1.0
    end type

    type, extends(base) :: child (n)
        integer, len :: n

        integer (k) :: ids(n) = -1
    end type
end module

program dtSpec004
use m
    class (base(k=4)), allocatable :: b1

    class (child(n=:, k=8)), pointer :: c1(:)

    logical(4), external :: precision_r4, precision_r8

    allocate (child(n=10, k=4):: b1)
    allocate (child(8, n=100) :: c1(10))

    b1%data = 1.2

    c1%data = (/(dtan(i*1.0d0), i=1, 10)/)

    do i = 1, 10
        c1(i)%ids = (/(i*1000+j, j=1,100)/)
    end do

    !! verify the results
    if (.not. precision_r4(1.2, b1%data)) error stop 1_4

    do i = 1, 10
        if (.not. precision_r8(c1(i)%data, dtan(i*1.0d0))) error stop 2_4

        do j = 1, 100
            if (c1(i)%ids(j) /= i*1000+j) error stop 3_4
        end do
    end do

    select type (x => b1)
        type is (child(n=*, k=4))
            do i = 1, 10
                if (x%ids(i) /= -1) error stop 4_4
            end do
        class default
            error stop 6_4
    end select
end
