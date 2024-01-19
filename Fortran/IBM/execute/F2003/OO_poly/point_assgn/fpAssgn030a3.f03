! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/21/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ASSOCIATE construct (array section used as the
!                               selector)
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
        integer*4 :: id = -1

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child
        character*20 :: name = 'default'

        contains

        procedure :: print => printChild
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
end module

program fpAssgn030a3
use m
    class (child), pointer :: c(:) => null()

    allocate (c(2:11))

    associate (x => c(::2)%base, x1 => c(3::2))
        x%id = 10

        x1%name = 'odd'

        print *, x
    end associate

    call c(10)%print
    call c(11)%print
end
