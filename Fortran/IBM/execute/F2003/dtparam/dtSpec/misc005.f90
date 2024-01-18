! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/13/2006
!*
!*  DESCRIPTION                : miscellaneous
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type A
        real(8) :: data(28)
    end type

    type, extends(A) :: B
        integer(8) :: ids(35)
    end type

    type (A), parameter :: a_const = A((/(i*1.0d0, i=1, 28)/))
    type (B), parameter :: b_const = B(1.0d2, (/(i,i=1, 35)/))

    type base
        class(A), allocatable :: data1(:)
        class(A), allocatable :: data2
    end type
end module

program dtparamDefVal003a
use m
    type (base), allocatable :: b2(:)
    logical(4), external :: precision_r8

    allocate(b2(10))

    do i = 1, 10, 2
        b2(i) = base((/(A((/(i*1.0d3+k*1.0d2+j, j=1, 28)/)), k=1,i)/), b_const)

        b2(i+1) = base((/(B(1.0d1, (/(j, j=1,35)/)), k=1, i+1)/), a_const)
    end do

    !! verify results
    do i = 1, 10, 2
        if (size(b2(i)%data1) /= i) error stop 1_4
        if (size(b2(i+1)%data1) /= i+1) error stop 2_4

        do k = 1, i
            do j = 1, 28
                if (.not. precision_r8(i*1.0d3+k*1.0d2+j, &
                        b2(i)%data1(k)%data(j))) error stop 3_4

                if (.not. precision_r8(b2(i+1)%data1(k)%data(j), 1.0d1)) &
                        error stop 4_4
            end do

            select type (x => b2(i+1)%data1)
                type is (B)
                    if (any(x(k)%ids /= (/(j, j=1,35)/))) error stop 5_4

                class default
                    error stop 6_4
            end select
        end do


        do j = 1, 28
            if (.not. precision_r8(b2(i)%data2%data(j), 1.0d2)) error stop 7_4

            if (.not. precision_r8(b2(i+1)%data2%data(j), j*1.0d0)) &
                    error stop 8_4
        end do

        select type (x => b2(i)%data2)
            class is (B)
                if (any (x%ids /= (/(j, j=1,35)/))) error stop 9_4

            class default
                error stop 10_4
        end select
    end do
end
