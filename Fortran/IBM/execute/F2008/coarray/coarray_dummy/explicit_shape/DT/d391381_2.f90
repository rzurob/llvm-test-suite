
    implicit none
    real(8), save :: x[*]
    real(8) y

    x = 1.0d0
    y = 1.0d0
    print *, 'outside:', x, y, x == y, x /= y

    call foo(x,y)

    contains

    subroutine foo (xx, yy)
        real(8) :: xx[*]
        real(8) :: yy

        if (xx /= yy) then
            print *, 'inside foo:',xx, yy, xx == yy, xx /= yy
            error stop 1
        end if
    end subroutine
    end
