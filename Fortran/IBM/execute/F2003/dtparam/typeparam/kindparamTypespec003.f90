
module m
    type base(k)
        integer, kind :: k = 4

        real(k) :: data(2) = -1.0
    end type
end module

program kindparamTypespec003
use m
    class(*), allocatable :: x1(:)
    class(*), pointer :: x2
    logical(4), external :: precision_r8

    allocate (base(4) :: x1(10))
    allocate (base(k=8) :: x2)

    !! assign values to x1
    select type (x => x1)
        type is (base(4))
            do i = lbound(x,1), ubound(x,1) - 1
                x(i)%data = (/((i*10 + j)*1.0e0, j =1,2)/)
            end do
        class is (base(8))
            error stop 1_4
        class default
            error stop 2_4
    end select

    !! assign values to x2
    select type (y => x2)
        class is (base(4))
            error stop 3_4
        class is (base(8))
            if ((.not. precision_r8 (y%data(1), real(-1.0, 8))) .or. &
                (.not. precision_r8 (y%data(1), real(-1.0, 8)))) error stop 4_4

            y%data = (/-1.2d0, -2.1d0/)
        class default
            error stop 5_4
    end select

    !! print the values
    select type (x1)
        class is (base(4))
            select type (x1)
                type is (base(4))
                    write (*, '(2f10.2)') x1
            end select
        type is (base(8))
            error stop 10_4
        class default
            error stop 11_4
    end select

    select type (x2)
        type is (base(4))
            error stop 12_4
        type is (base(8))
            write (*, '(2f12.5)') x2
        class default
            error stop 13_4
    end select
end
