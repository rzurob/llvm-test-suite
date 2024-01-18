
module m
    type base (k, n)
        integer, kind :: k = 4
        integer, len :: n = 33

        complex(k) :: cx(n) = (-1.0, -1.0)
    end type

    contains

    recursive subroutine updateVal (b, cx, i)
        class(base), intent(inout) :: b
        complex(4), intent(in) :: cx(*)
        integer, intent(in) :: i

        if ((i <= 0) .or. (i > b%n)) return

        b%cx(i) = sqrt(cx(i))

        call updateVal (b, cx, i+1)
    end subroutine
end module

program dtparamDefVal008
use m
    class(base), pointer :: b1(:)

    complex(4) :: cx1(40, 10)

    logical(4), external :: precision_x8

    allocate (b1(10))

    do j = 1, 10
        do i = 1, 40
            cx1(i,j) = (i*1.0e1+j-1, i*1.0e1+j-1)
        end do
    end do

    do i = 1, 10
        call updateVal (b1(i), cx1(:,i), 1)
    end do

    !! verify results
    do i = 1, 10
        do j = 1, 33
            if (.not. precision_x8 (b1(i)%cx(j), sqrt((j*1.0e1+i-1, &
                j*1.0e1+i-1)))) error stop 1_4
        end do
    end do
end
