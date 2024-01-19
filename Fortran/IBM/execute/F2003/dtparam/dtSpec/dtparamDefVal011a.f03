
module m
    type base (k, n)
        integer, kind :: k = 8, n= 45

        real(k) :: data (n)

        contains

        final :: finalizeBase
    end type

    contains

    elemental subroutine finalizeBase (b)
        type (base), intent(inout) :: b

        b%data = sqrt(-1.0d0)
    end subroutine
end module

program dtparamDefVal011
use ieee_arithmetic
use m
    class(base), allocatable :: b1(:,:)

    allocate (base:: b1(10, 20))

    do j = 1, 20
        do i = 1, 10
            b1(i, j)%data = (/(i*1.0e4+j*1.0e2+k, k=1, 45)/)
        end do
    end do

    call resetBaseVal(b1)

    !! verify that the values of b1 are set to NaN
    do j = 1, 20
        do i = 1, 10
            do k = 1, 45
                if (.not. ieee_is_nan(b1(i,j)%data(k))) error stop 1_4
            end do
        end do
    end do
end

subroutine resetBaseVal (b)
use m
    type (base), intent(out) :: b(200)
end subroutine
