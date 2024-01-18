
module m
    type, abstract :: base
        procedure(genBasePtr), pointer :: replicate => null()
    end type

    type, extends(base) :: child (k)
        integer, kind :: k

        real(k), allocatable :: data(:)
    end type

    abstract interface
        class(base) function genBasePtr (b)
        import
            pointer genBasePtr
            class(base), intent(in) :: b
        end function
    end interface

    interface setupBase
        module procedure setupBase
        module procedure setupBase8
    end interface

    contains

    subroutine setupBase (b, r1)
        class(base), intent(out) :: b
        real(4), intent(in) :: r1(:)

        select type (b)
            type is (child(4))
                allocate (b%data(size(r1)), source=r1)
            class default
                stop 10
        end select
    end subroutine

    subroutine setupBase8 (b, r1)
        class(base), intent(out) :: b
        real(8), intent(in) :: r1(:)

        select type (b)
            type is (child(8))
                allocate (b%data(size(r1)), source=r1)
            class default
                stop 20
        end select
    end subroutine

    subroutine printBase (b)
        class(base), intent(in) :: b

        select type (b)
            type is (child(4))
                if (allocated(b%data)) then
                    write(*, '(5f10.2)') b%data
                end if

            type is (child(8))
                if (allocated(b%data)) then
                    write(*, '(3g15.5)') b%data
                end if
            class default
                stop 30
        end select
    end subroutine
end module

program dtparamProcComp006
use m
    class(base), pointer :: b1, b2
    class(base), allocatable, target :: b3

    double precision d1(100)

    procedure (genBasePtr) createChild4, createChild8

    d1 = (/(i*1.0d0*dsin(i*1.0d0), i=1, 100)/)

    allocate (child(4) :: b1)
    allocate (child(8) :: b3)

    b2 => b3

    call setupBase(b1, (/(i*1.0, i = 1, 10)/))
    call setupBase(b2, d1)

    b1%replicate => createChild4
    b2%replicate => createChild8

    b2 => b1%replicate()

    call printBase (b2)

    deallocate (b2)

    b2 => b3%replicate()

    call printBase (b2)

    deallocate (b2, b1, b3)
end

function createChild4 (b)
use m, only : base, child
    class (base), pointer :: createChild4
    class (base), intent(in) :: b

    select type (b)
        type is (child(4))
            allocate (createChild4, source=b)
        class default
            error stop 15_4
    end select
end function

function createChild8 (b)
use m, only : base, child
    class (base), pointer :: createChild8
    class (base), intent(in) :: b

    select type (b)
        type is (child(8))
            allocate (createChild8, source=b)
        class default
            error stop 25_4
    end select
end function
