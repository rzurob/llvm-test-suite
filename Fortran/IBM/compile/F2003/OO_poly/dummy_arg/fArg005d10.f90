! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/06/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (if the dummy-arg is an
!*                               allocatable, the actual arg shall be an
!*                               allocatable)
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
    end type
end module

program fArg005d10
use m
    class (base), pointer :: b1
    class (*), pointer :: x1

    call abc (b1)

    call cba (x1)

    contains

    subroutine abc (b)
        class (base), allocatable :: b
    end subroutine

    subroutine cba (x)
        class (*), allocatable :: x
    end subroutine
end
