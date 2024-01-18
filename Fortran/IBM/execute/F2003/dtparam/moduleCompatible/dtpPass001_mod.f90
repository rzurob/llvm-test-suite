
module m
    type point (k, dim)
        integer, kind :: k
        integer, len :: dim

        real(k) coor(dim)

        contains

        procedure :: length4
    end type

    contains

    real(4) function length4 (p1, p2)
        class(point(4,*)), intent(in) :: p1
        class(point(4,p1%dim)), intent(in) :: p2

        implicit none

        integer i

        length4 = 0

        do i = 1, p1%dim
            length4 = length4 + (p1%coor(i) - p2%coor(i))**2
        end do

        length4 = sqrt(length4)
    end function
end module

