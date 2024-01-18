
module m
    type base (k, n)
        integer, kind :: k = 4, n=10

        real(k) :: data(n) = 1.0
    end type

    integer, parameter :: i_const = 20

    contains

    type(base(8, i_const)) function genBase8 (data)
        real(8), intent(in) :: data(i_const)

        genBase8%data = data
    end function
end module

program kindparamProcInterf001
use m
    procedure(type(base(4, 3))) p1
    procedure(type(base(8, 20))), pointer :: p2

    real(8) :: d1(40, 20)
    type (base(8, i_const)) b1
    type (base(4,3)), allocatable :: b2

    logical(4), external :: precision_r8, precision_r4

    allocate (b2)
    call random_number (d1)

    p2 => genBase8

    b1 = p2(d1((/30, 15/), ::2))

    b2 = p1(12.1e0)

    !! verify
    do i = 1, 20, 2
        if ((.not. precision_r8(b1%data(i), d1(30, i))) .or. &
            (.not. precision_r8(b1%data(i+1), d1(15, i)))) error stop 1_4
    end do

    if ((.not. precision_r4(b2%data(1), 1.21e1)) .or. &
        (.not. precision_r4(b2%data(2), 1.31e1)) .or. &
        (.not. precision_r4(b2%data(3), 1.41e1))) error stop 2_4
end


type(base(4, 3)) function p1 (data)
use m, only: base
    real(4), intent(in) :: data

    p1%data = (/data, data+1.0, data+2.0/)
end function
