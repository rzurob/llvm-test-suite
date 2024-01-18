!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/20/2006
!*
!*  DESCRIPTION                : miscellaneous (defect 312629)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        procedure(p), pointer :: p1 => null()
    end type

    type, extends(base) :: child
        integer :: id
        character(25) :: name
    end type

    abstract interface
        subroutine p (b)
        import
            class(base), intent(in) :: b
        end subroutine
    end interface
end module

program misc007
use m
    class(base), allocatable :: b1(:)

    procedure(p) printBase

    allocate (b1(10), source=(/(child(printBase, i, &
        name='b1('//char(ichar('0')+i)//')'), i=0,9)/))

    !! verify
    do i = 1, 10
        call b1(i)%p1()
    end do
end

subroutine printBase (b)
use m
    class (base), intent(in) :: b

    select type (b)
        type is (base)
            print *, 'empty type'

        class is (child)
            print *, b%id, b%name

        class default
            error stop 10_4
    end select
end subroutine
