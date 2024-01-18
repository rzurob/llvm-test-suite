
module m
    implicit none
    integer(8), save :: a(100)[*] = -1
end module m

module m1
    implicit none
    contains

    subroutine assign_val (val)
        use m, only : a
        integer(8), intent(in) :: val(:)

        integer :: me
        me = this_image()

        if (me == 2) a(::10)[1] = val(:10)
        sync all
    end subroutine
end module

use m1
use m
    implicit none
    integer(8) :: a2(10)

    integer me, i, np

    np = num_images()
    me = this_image()

    if (np < 2) then
        print *, 'Error: the program requires at least 2 images to run'
        error stop 1
    end if
    a2 = [(i, i = 1, 10)]

    call assign_val (a2)

    if (me == 1) print *, a
end

