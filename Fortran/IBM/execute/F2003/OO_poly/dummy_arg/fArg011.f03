! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/23/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (dummy-arg as selector in
!                               ASSOCIATE construct)
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
        integer*4 :: id

        contains

        procedure :: print => printBase
    end type

    type, extends (base) :: child
        character*20 :: name

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

    subroutine printBaseInAssociate (b)
        class (base), intent(in) :: b

        associate (x => b)
            call x%print
        end associate
    end subroutine
end module

program fArg011
use m
    type (base) :: b1 = base (1)

    type (child) :: c1 = child (2, 'c1')

    class (base), pointer :: b_ptr

    allocate (b_ptr, source=child(3,'b_ptr'))

    call printBaseInAssociate (b1)

    call printBaseInAssociate (c1)

    call printBaseInAssociate (b_ptr)
end