
module m
    type point (k)
        integer, kind :: k

        real(k) :: x, y
    end type

    type (point(4)), parameter :: originK4 = point(4)(0.0, 0.0)
    type (point(8)) originK8

    parameter (originK8 = point(8)(0.0d0, 0.0d0))
end module

program dtparamUnlmtPoly003
use m
    type container
        class(*), allocatable :: data
    end type

    class (*), pointer, dimension(:) :: x

    type (container) co1(100)

    allocate (x(2), source=(/point(8)(1.0d1, 2.0d1), point(8)(1.0d0, 2.0d0)/))

    co1(1) = container (originK8)

    co1(2) = container (originK4)

    co1(8) = container (x(2))
    co1(9) = container (x(1))

    do i = 1, 2
        call printContainer (co1(i))
        call printContainer (co1(i+7))
    end do

    contains

    subroutine printContainer (co)
        class(container), intent(in) :: co

        select type (x => co%data)
            type is (point(4))
                write (*, '(2f10.2)') x

            type is (point(8))
                write (*, '(2f15.5)') x

            class default
                print *, 'undefined type'
        end select
    end subroutine
end
