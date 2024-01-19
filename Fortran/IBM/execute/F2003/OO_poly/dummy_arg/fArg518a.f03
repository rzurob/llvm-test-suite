! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/03/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (sequence association for
!                               the unlimited poly dummy-arg)
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

    subroutine testSeqAsso (x)
        class (*), intent(in) :: x(4)
    end subroutine
end module

program fArg518a
use m
    character :: str1 (7) = (/'x','l','f','t','e','s','t'/)

    call testSeqAsso (str1(3))
end
