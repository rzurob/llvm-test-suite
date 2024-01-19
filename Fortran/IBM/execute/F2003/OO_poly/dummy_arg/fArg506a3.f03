! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/06/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (components' default
!                               initialization for INTENT(OUT) dummy-arg)
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
        integer(4) :: id = -1
    end type

    type container
        type (base) :: b1(3)
    end type

    contains

    subroutine abc (co)
        class (container), intent(out) :: co

        if (any (co%b1%id /= -1)) error stop 1_4
    end subroutine

    subroutine cba (x)
        class (*), intent(out) :: x
    end subroutine
end module

program fArg506a3
use m
    type (container) :: co1

    if (any (co1%b1%id /= -1)) error stop 5_4

    co1%b1%id = (/1,2,3/)

    call abc (co1)

    co1%b1%id = (/1,2,3/)

    call cba (co1)

    if (any (co1%b1%id /= -1)) error stop 6_4
end
