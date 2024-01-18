
module m
    type base (n)
        integer, len :: n

        private

        integer :: ids (n)
    end type

    type, extends(base) :: child (l)
        integer, len :: l

        private

        character(l) :: name
    end type

    interface base
        module procedure genBase
    end interface

    interface child
        module procedure genChild
    end interface

    contains

    function genBase (n, ids)
        integer, intent(in) :: n, ids(n)
        type(base(n)) genBase

        genBase%ids = ids
    end function

    function genChild (n, ids, name)
        integer, intent(in) :: n, ids(n)
        character(*), intent(in) :: name
        type (child(n, len(name))) genChild

        genChild%base = base(n, ids)
        genChild%name = name
    end function

    subroutine printBase (b)
        class (base(*)), intent(in) :: b

        select type (b)
            type is (base(*))
                print *, b%ids

            type is (child(*,*))
                print *, b%ids, '|', b%name
            class default
                stop 10
        end select
    end subroutine
end module

program dtparamCompAccess005
use m
    type(child(10, 20)) :: c1
    type(base(20)) :: b1

    integer i1(100)

    i1 = (/(j*10, j = 1, 100)/)

    b1 = base(20, i1(::2))

    c1 = child (10, i1, 'test on dtparameters')

    call printBase(b1)
    call printBase(c1)
end
