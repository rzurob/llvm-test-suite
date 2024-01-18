
!defect 318884
module m
    type base
        character(:), allocatable :: names
    end type
end module

use m
    type(base), allocatable :: b1(:)

    character(:), pointer :: c1

    allocate(b1(5))

    allocate(c1, source='where to start?')

    b1(1) = base('abcd')
    b1(2) = base('xlf'//'test')
    b1(3) = base(c1)

    b1(4) = b1(2)

    b1(5) = base(b1(2)%names//' '//b1(3)%names)

    do i = 1, 5
        print *, b1(i)%names
    end do
end
