! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/11/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (OPTIONAL dummy-arg used
!                               as actual-arg that is also OPTIONAL)
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
    private internalSub
    contains

    subroutine test (x)
        class (*), optional :: x

        call internalSub (x)
    end subroutine

    subroutine internalSub (x)
        class (*), optional :: x

        print *, present(x)
    end subroutine
end module

program fArg032
use m
    call test (100)

    call test
end
