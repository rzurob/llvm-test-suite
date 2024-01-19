
    implicit none
    real, save :: x(10)[*]
    logical, external :: precision_r4

    integer me, i

    me = this_image()

    x = foo(10)

    do i = 1, 10
        if (.not. precision_r4(x(i), i*1.0*me)) then
            print *, 'Failed to verify'
            print *, x(i), 'vs', i*1.0*me
            error stop 1
        end if
    end do

    contains
    function foo(n)
        integer, intent(in) :: n
        real foo (n)
        integer i

        foo = [(i*this_image(), i = 1, 10)]
    end function
    end
