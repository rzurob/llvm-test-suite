
module m
    type point (dim)
        integer, len :: dim

        real coord(dim)
    end type

    type, extends(point) :: colorPoint (k)
        integer, kind :: k

        integer(k) :: color
    end type

    type, extends(colorPoint) :: hiLitePoint(n)
        integer, len :: n

        character(n) :: desc
    end type
end module

module n
use m
    type base
        class(point(:)), allocatable :: data(:)
    end type
end module

program dtparamConstr057a
use n
    type(base), parameter :: b1Const = base(null())

    type(base) b1

    logical(4), external :: precision_r4

    enum, bind(c)
        enumerator :: red = 1, yellow, blue,black
    end enum

    integer color
    character(20), parameter :: colorDesc(4) =[character(20) ::'RED', &
        'YELLOW', 'BLUE', 'BLACK']

    b1 = base(data=[(hiLitePoint(2,4,len(colorDesc)) ([1.0*i, 2.0*i], &
        desc=colorDesc(mod(i-1,4)+1), color=mod(i-1,4)+1), i=1, 100)])

    !! verify b1
    do i = 1, 100
        if (.not. precision_r4(b1%data(i)%coord(1), i*1.0_4)) error stop 1_4

        if (.not. precision_r4(b1%data(i)%coord(2), 2.0_4*i)) error stop 2_4

        select case (mod(i,4))
            case (1)
                color = red
            case (2)
                color = yellow
            case (3)
                color = blue
            case (0)
                color = black
        end select

        select type (x => b1%data(i))
            type is (hiLitePoint(*,4,*))
                if (x%color /= color) error stop 3_4

                if (x%desc /= colorDesc(color)) error stop 4_4

            class default
                error stop 10_4
        end select
    end do
end
