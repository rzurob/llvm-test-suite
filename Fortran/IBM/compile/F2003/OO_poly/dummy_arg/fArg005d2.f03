! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/03/2004
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
        type (base), allocatable :: b1
    end subroutine

    subroutine cba (b2)
        type (base), allocatable :: b2(:)
    end subroutine
end module

program fArg005d2
use m
    class (base), allocatable :: c1
    class (base), allocatable :: c2(:)

    call abc (c1)

    call cba (c2)
end
