
module m
    type dataType
        class(*), allocatable :: x(:)
    end type

    type base
        class (dataType), allocatable :: data
    end type
end module

program dcmlChildWrite005
use m
    logical(4), external :: precision_r4
    type(base), allocatable :: b1(:)

    allocate (b1(12))

    do i = 1, 12, 3
        allocate(b1(i+2)%data)
        allocate(base::b1(i+2)%data%x(1))

select type (x=>b1(i+2)%data%x(1)); type is(base); allocate(x%data); allocate(x%data%x(i+2), source=sin((/(j*1.0, j=1, i+2)/)))

            class default
                stop 30
        end select
    end do

    do i = 1, 12, 3
        select type (x => b1(i+2)%data%x(1))
            type is(base)
                select type (y => x%data%x)
                    type is (real)
                        do j = 1, size(y)
                            if (.not. precision_r4(y(j), sin(j*1.0))) then
                                print *, i, j, y(j), sin(j*1.0)

                                stop 1
                            end if
                        end do

                    class default
                        stop 20
                end select

            class default
                stop 10
        end select
    end do

end
