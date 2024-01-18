
module m
    type base (k, l)
        integer, len :: l
        integer, kind :: k

        real(k) :: data(l)
    end type

    type, extends(base) :: child
        complex(k) :: cx
    end type

    type, extends(child) :: gen3 (ln)
        integer, len :: ln

        character(ln) :: name
    end type
end module

module printStruct
use m
    contains

    subroutine printBase4 (b)
        type (base(4, *)), intent(in) :: b

        write (*, 100) b

100     format (5g15.5)
    end subroutine


    subroutine printBase8 (b)
        type (base(8,*)), intent(in) :: b

        write (*, 100) b

100     format (5g16.9)
    end subroutine

    subroutine printChild4 (c)
        type (child(4,*)), intent(in) :: c

        write (*, 100, advance='no') c%data
        write (*, 101) c%cx

100     format (5g15.5, '; ')
101     format ('(', g15.5, ',', g15.5, ')')
    end subroutine

    subroutine printGen3_8 (g)
        type (gen3(8,*,*)), intent(in) :: g

        write (*, 100, advance='no') g%data
        write (*, 101, advance='no') g%cx
        write (*, '(a)') g%name

100     format (5g16.9, '; ')
101     format ('(', g16.9, ',', g16.9, ')')
    end subroutine
end module

program typeParamOrder002
use m
use printStruct
    class(base(4,:)), allocatable :: b1(:), b2
    class (child(8,:)), pointer :: c1

    real(8) d1(20)

    d1 = dsqrt((/(i*1.0d0, i=1,20)/))

    allocate (base(4,10) :: b1(10))

    allocate (b2, source=child(4, 12) ((/(i*1.0, i=1, 12)/), (15.0, 16.0)))
    allocate (c1, source=gen3(8,15,21)(d1(:15), (d1(16), d1(17)), &
                'c1 of gen3 type'))

    do i = 1, 10
        b1(i)%data = (/((i-1)*10+j, j=0, 9)/)
    end do

    !! print out the values
    do i = 1, 10
        call printBase4 (b1(i))
    end do

    select type (b2)
        type is (child(4, *))
            call printChild4 (b2)

        class default
            error stop 1_4
    end select

    select type (c1)
        class is (gen3(8,*,*))
            call printGen3_8(c1)

            print *, 'its parent component is'
            call printBase8 (c1%base)

        class default
            error stop 2_4
    end select
end
