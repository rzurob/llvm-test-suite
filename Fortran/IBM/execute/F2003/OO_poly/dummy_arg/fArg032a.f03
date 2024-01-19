! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/11/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : arguemnt association (OPTIONAL dummy-arg used
!                               as actual arg for an OPTIONAL dummy-arg)
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
    contains

    subroutine test1 (i)
        integer*4, optional :: i

        call internalSub (i, present(i))
    end subroutine

    subroutine internalSub (x, l)
        class (*), optional :: x
        logical, intent(in) :: l

        if (present(x) .neqv. l) error stop 1_4
    end subroutine
end module

program fArg032a
use m

    call test1
end
