! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/16/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : IMPLICIT (implied entity's binding can be
!*                               invoked)
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
        integer*4 :: id = 1

        contains

        procedure :: print => printBase
        procedure, pass (b) :: addId => addVal2ID
    end type

    contains

    subroutine printBase(b)
        class (base), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine addVal2ID (i, b)
        class (base), intent(inout) :: b
        intent(in) ::i

        b%id = b%id + i
    end subroutine
end module

program fimplct007
use m
    implicit type(base) (b)

    call b1%print

    call b2%addId (9)

    call b2%print
end
