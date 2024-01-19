! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/04/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (the actual arg shall be
!*                               polymorphic if and only if the dummy-arg is
!*                               polymorphic)
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
    end type

    type, extends(base) :: child
        character*20 :: name
    end type

    contains

    subroutine abc (b1)
        class (base), pointer :: b1
    end subroutine

    subroutine cba (b2)
        class (base), pointer :: b2(:)
    end subroutine
end module

program fArg005d4
use m
    type (base), pointer :: c1
    type (base), pointer :: c2(:)

    type (child), pointer :: c3

    call abc (c1)

    call cba (c2)

    call abc (c3)
end
