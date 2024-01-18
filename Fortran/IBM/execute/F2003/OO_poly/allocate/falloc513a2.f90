
module m
    type base
        integer, allocatable :: data(:)
    end type

    type, extends(base) :: child
        character(20), allocatable :: names(:)
    end type
end module

program falloc513a2
use m
    class (base), pointer :: b1(:,:)
    class (*), allocatable :: x1(:,:)

    allocate (b1(2,-2), source= reshape((/child((/1/), null())/), (/2,0/)))
    allocate (x1(1:-2, 2), source=reshape ((/child(null(), (/'abc', 'xyz'/))/), &
            (/0, 2/)))

    if ((.not. associated (b1)) .or. (.not. allocated(x1))) error stop 1_4

    if (.not. same_type_as (b1, x1)) error stop 2_4

    if (any (shape(b1) /= (/2, 0/))) error stop 3_4
    if (any (shape (x1) /= (/0,2/))) error stop 4_4

    select type (x1)
        class is (child)
        class default
            error stop 5_4
    end select
end
