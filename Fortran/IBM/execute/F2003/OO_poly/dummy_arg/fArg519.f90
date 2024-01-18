! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/02/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (unlimited-poly-dummy-arg
!                               with INTENT(OUT) will cause the actual-arg to be
!                               undefined except for those components having
!                               default-initialization)
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
        integer*4 :: id = -1
    end type

    contains

    subroutine abc (x)
        class (*), intent(out) :: x
    end subroutine
end module

use m
    type (base) :: b1 = base (10)

    call abc (b1)

    if (b1%id /= -1) error stop 1_4
end
