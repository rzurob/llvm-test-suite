! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/02/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (array section with vector
!                               subscript can not be associated with
!                               INTENT(INOUT) dummy-arg)
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

    subroutine test1 (x)
        class(*), intent(inout) :: x (:)
    end subroutine
end module

program fArg022d2
use m
    integer*4 i1 (5), vec(2)

    vec = (/1,1/)

    call test1 (i1(vec))
end
