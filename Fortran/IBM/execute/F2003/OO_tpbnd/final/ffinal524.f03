! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/16/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : final sub (other bindings can be called in the
!*                               final sub; also uses IMPLICIT)
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
        integer*4 :: id = 0

        contains

        final :: finalizeBase
        procedure :: print => printBase
    end type

    contains

    subroutine finalizeBase (b)
        implicit type (base) (b)

        intent(in) :: b

        call b%print
        print *, 'end finalizeBase'
    end subroutine

    subroutine printBase (b)
        class (base), intent(in) :: b

        print *, b%id
    end subroutine
end module

program ffinal524
    call abc
end

subroutine abc
use m
    implicit type(base) (b)

    target b1

    b1%id = 10
end