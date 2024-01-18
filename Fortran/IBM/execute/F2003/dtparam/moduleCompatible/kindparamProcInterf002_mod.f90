
module m
    integer, parameter :: range1 = 28, range2 = 99, prec1 = 5, prec2 = 12

    type base (k)
        integer, kind :: k

        real(k), allocatable :: data(:)
        character(20) :: name = 'default'
    end type

    type baseGenerator
        procedure(type(base(selected_real_kind(prec1, range1)))), nopass, &
                pointer :: gen1 => null()

        procedure(type(base(selected_real_kind(prec2, range2)))), nopass, &
                pointer :: gen2 => null()
    end type
end module

