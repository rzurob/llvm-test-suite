! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/03/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (undefinable actual-arg
!                               tries to be associated with INTENT(INOUT)
!                               dummy-arg)
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

    subroutine abc (x)
        class (*), intent(inout) :: x(:)

        print *, size(X)
    end subroutine
end module


program fArg021d1
use m
    integer*4 :: i_v (3)

    integer*4, parameter :: i_c (3) = (/1,2,3/)

    call abc (i_c(::2)) !<-- illegal
    call abc ((/i_c/))  !<-- illegal

    call abc ((/i_v(1:2)/)) !<-- illegal
    call abc ((/i_v/))  !<-- illegal
end