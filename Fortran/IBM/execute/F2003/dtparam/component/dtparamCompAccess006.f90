!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/30/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.3.5: comp. access.)
!                               Case: Private component statement; use generic
!                               name to overrids the structure constructor; use
!                               the overriden type name in allocate statement
!                               and procedure call.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

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

    function genChild (n, ids, l, name)
        integer, intent(in) :: n, l, ids(n)
        character(*), intent(in) :: name
        type (child(n, l)) genChild

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

program dtparamCompAccess006
use m
    class (base(:)), pointer :: b1

    integer i1(100)

    i1 = (/(j, j = 100, 1, -1)/)

    allocate (b1, source=&
        child(10, (/(i, i=1, 100)/), 20, 'xlftest on dtparameters'))

    call printBase(b1)

    call printBase(base(30, i1))

    call printBase (child(5, i1(:5), 10, 'xlftest again'))
end
