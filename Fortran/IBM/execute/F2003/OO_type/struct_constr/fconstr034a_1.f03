! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/20/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : structure constructor (unlimited
!                               poly-allocatable scalar component; data-source
!                               is of derived type with bindings, including
!                               final binding)
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type container
        class(*), allocatable :: data
    end type
end module

module n
    type base
        integer(8) :: id

        contains

        procedure :: print => printBase
        final :: finalizeBase
    end type

    type, extends(base) :: child
        character(18) :: name

        contains

        procedure :: print => printChild

        final finalizeChild
    end type

    contains

    subroutine printBase (b)
        class (base), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child), intent(in) :: b

        print *, b%id, b%name
    end subroutine

    subroutine finalizeBase (b)
        type(base) b

        print *, 'finalizeBase'
    end subroutine

    subroutine finalizeChild (c)
        type (child) c

        print *, 'finalizeChild'
    end subroutine
end module

program fconstr034a_1
use n
use m
    class(*), allocatable :: x1
    class (*), pointer :: x2(:)

    type (child), target :: c1 (2:10)

    allocate (x1, source=child(100, 'x1'))

    c1%id = (/(i,i=2,10)/)
    c1%name = 'c1_array_of_9'

    x2 => c1

    print *, 'begin'

    associate (y1 => container(x1), y2 => container(x2(5)))
        select type (z => y1%data)
            class is (base)
                call z%print
        end select

        select type (z => y2%data)
            class is (base)
                call z%print
        end select
    end associate

    print *, 'end'
end
