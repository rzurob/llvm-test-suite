module m
    type base (k1, l1)
        integer, kind :: k1
        integer, len :: l1

        integer(k1) :: id = -1
        character(l1) :: name = 'nameless'
    end type

    type, extends(base) :: child
        logical(k1) :: flag
    end type

    integer, parameter :: single = 4
    integer, parameter :: double = 8
    integer, parameter :: nameLen = 6

    class(base(single, nameLen)), pointer :: b1_m => null()
end module


module m1
use m
    type, extends(child) :: gen3
        real(k1*2) :: data(l1) = 1.0_8
    end type
end module

