! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/12/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (select type construct for
!                               dummy arguments)
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

    subroutine abc (a)
        class (*) :: a

        select type (a)
            type is (integer(4))
                a = 10
            class default
                error stop 1_4
        end select
    end subroutine
end module

program fArg009a3
use m
    integer*4 :: i1 (3:7)

    call abc (i1(4))

    if (i1(4) /= 10) error stop 2_4
end
