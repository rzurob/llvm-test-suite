
module m
    type base(n1,k1,k2)    ! (20,8,4)
        integer, kind         :: k1,k2
        integer, len          :: n1
        real(k1), allocatable :: data(:)
        logical(k2), pointer  :: mask(:)

        contains

        procedure :: setMask => setBaseMask
        procedure :: reduce => reduceBaseData
    end type

    contains

    subroutine copyBase (b1, b2)
        type(base(:,8,4)), allocatable :: b1, b2

        b1 = b2
    end subroutine

    subroutine setBaseMask (b, d1, cmp)
        class(base(*,8,4)) b
        double precision d1
        procedure(logical) cmp

        if (allocated(b%data)) then
            allocate(b%mask(size(b%data)))

            b%mask = [(cmp(b%data(i), d1),&
                i=lbound(b%data,1), ubound(b%data,1))]
        else
            nullify(b%mask)
        end if
    end subroutine

    subroutine reduceBaseData (b)
        class(base(*,8,4)) b

        if (associated(b%mask)) then
            b%data = pack(b%data, b%mask)

            nullify(b%mask)
        else
            stop 10
        end if
    end subroutine
end module

module m1
use m, only: base
    type dataType(k3,n2)    ! (4,20)
        integer, kind :: k3
        integer, len  :: n2
        double precision, allocatable :: data(:)
    end type
    contains

    subroutine moveData (d1, b1)
        type(base(:,8,4)), allocatable :: b1
        class(dataType(4,*)) d1

        if (.not. allocated(b1)) allocate(base(20,8,4) :: b1)

        call move_alloc (d1%data, b1%data)
    end subroutine

    subroutine setMask (b1, d1, func)
        type(base(:,8,4)), allocatable :: b1
        real(8) d1
        procedure(logical) func

        call b1%setMask(d1, func)
    end subroutine
end module

