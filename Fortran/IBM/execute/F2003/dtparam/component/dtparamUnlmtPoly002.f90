
module m
    type generalPoint (n)
        integer, len :: n

        real :: coord(n)
    end type

    type color (k)
        integer, kind :: k

        integer(k) :: colorVal
    end type

    type, extends(generalPoint) :: colorPoint (ck)
        integer, kind :: ck

        type(color(ck)) :: c
    end type

    contains

    subroutine printPoint (p)
        class(generalPoint(*)), intent(in) :: p

        select type (p)
            type is (generalPoint(*))
                write (*, '(5f10.2)') p

            type is (colorPoint(*, 1))
                write (*, '(5f10.2)', advance='no') p%coord
                write (*, '(i3)') p%c

            type is (colorPoint(*, 2))
                write (*, '(5f10.2)', advance='no') p%coord
                write (*, '(i6)') p%c

            type is (colorPoint(*, 4))
                write (*, '(5f10.2)', advance='no') p%coord
                write (*, '(i12)') p%c

            class default
                stop 10
        end select
    end subroutine
end module

module m1
    type container
        class(*), allocatable :: data1(:)
        class(*), pointer :: data2(:,:)
    end type
end module

program dtparamUnlmtPoly002
use m1
use m
    type (container), allocatable :: co1(:)
    class(*), pointer :: x2(:,:)
    class(*), allocatable :: x1(:)

    allocate (co1(20))

    allocate (x1(0:1), source=(/generalPoint(2)((/1.0, 2.0/)), &
            generalPoint(2)((/3.0, 4.0/))/))

    allocate (x2(2,0:1), source=reshape((/generalPoint(3)((/1.5, 2.5, 3.5/)), &
        generalPoint(3)((/4.5, 5.5, 6.5/)), generalPoint(3)(3.3),&
        generalPoint(3)((/7.5, 8.5, 9.5/))/), (/2,2/)))

    co1(2) = container(x1, x2)

    allocate (co1(5)%data1(1), source=(/colorPoint(2, 1)((/5.0, 6.0/), &
            color(1)(2))/))

    allocate (co1(5)%data2(1,0:0), source=colorPoint(3,1)((/10.5, 11.5, 12.5/), &
            color(1)(5)))

    allocate (co1(8)%data1(2), source=colorPoint(3,2)((/13.5, 14.5, 15.5/), &
                color(2)(255)))

    allocate (co1(8)%data2(2,1), source=colorPoint(2,2)((/7.0, 8.0/), &
                color(2)(303)))

    co1(4:10:3) = co1(2:8:3)

    if (.not. associated(co1(2)%data2, x2)) error stop 5_4

    do i = 4, 10, 3
        if (.not. allocated(co1(i)%data1)) error stop 1_4
        if (.not. associated(co1(i)%data2, co1(i-2)%data2)) error stop 2_4

        call printX (co1(i)%data1, size(co1(i)%data1))
        call printX (co1(i)%data2, size(co1(i)%data2))

        deallocate (co1(i)%data1, co1(i)%data2)
    end do

    contains

    subroutine printX (x, lb)
        class(*), intent(in) :: x(*)
        integer, intent(in) :: lb

        do j = 1, lb
            select type (y => x(j))
                class is (generalPoint(*))
                    call printPoint (y)

                class default
                    stop 20
            end select
        end do
    end subroutine
end
