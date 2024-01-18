
module m
    type base (k)
        integer, kind :: k

        real(k) data
        integer(2) id
    end type

    interface print
        module procedure printBase4
        module procedure printBase8
    end interface

    contains

    subroutine printBase4(b)
        type (base(k=4)), intent(in) :: b(:)

        print *, 'base type, kind of:', b%k

        do i = 1, size(b)
            print *, b(i)%data, b(i)%id
        end do
    end subroutine

    subroutine printBase8(b)
        type (base(k=8)), intent(in) :: b(:)

        print *, 'base type, kind of:', b%k

        do i = 1, size(b)
            print *, b(i)%data, b(i)%id
        end do
    end subroutine
end module

program kindparamTypespec004
use m
    call print ((/base(k=8) ::/))
    call print ([ base(4) ::])
end
