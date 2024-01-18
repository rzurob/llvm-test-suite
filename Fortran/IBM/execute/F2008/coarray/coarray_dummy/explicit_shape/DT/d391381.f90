
    implicit none
    integer(8), save :: x[*]
    integer(8) y

    x = 8_8
    y = 8_8

    if (x /= y) error stop 1

    call foo(x,y)

    contains

    subroutine foo (xx, yy)
        integer(8) :: xx[*]
        integer(8) :: yy

        if (xx /= yy) then
            print *, xx, yy, xx == yy, xx /= yy
            error stop 10
        end if
    end subroutine
    end
