! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/10/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (dummy_arg used as actual
!                               arg)
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
    type base
        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child
        integer*4 :: id

        contains

        procedure :: print => printChild
    end type

    type, extends (child) :: gen3
        character*20 :: name

        contains

        procedure :: print => printGen3
    end type

    contains

    subroutine printBase (b)
        class (base), intent(in) :: b

        print *, 'empty type'
    end subroutine

    subroutine printChild (b)
        class (child), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printGen3 (b)
        class (gen3), intent(in) :: b

        print *, b%id, b%name
    end subroutine

    subroutine printData (c)
        class (child), intent(in) :: c

        call printBaseData (c)
    end subroutine

    subroutine printBaseData (b)
        class (base), intent(in) :: b

        call b%print
    end subroutine

    subroutine printPtrData (c)
        class (child), pointer, intent(in) :: c

        call printBaseData (c)
    end subroutine
end module

program fArg031
use m
    class (child), pointer :: c1

    type (gen3), target :: g1 = gen3 (-1, 'g1')

    allocate (c1, source=gen3 (100, 'c1'))

    call printData (gen3(1, 'gen3_1'))

    call printData (child (10))

    call printData (c1)

    call printPtrData (c1)

    deallocate (c1)

    c1 => g1

    call printPtrData (c1)
    call printData (c1)
end
