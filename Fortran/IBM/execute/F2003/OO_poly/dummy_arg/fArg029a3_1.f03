! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 08/09/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (unlimited poly-function
!                               return as the actual-arg; use BIND(C) type to
!                               test)
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
    use ISO_C_BINDING

    type, bind(c) :: bType
        integer(C_int) :: i1
        integer(c_short) :: i2
    end type

    contains

    class (*) function replicateAnything (x)
        class (*), intent(in) :: x

        allocatable replicateAnything

        allocate (replicateAnything, source=x)
    end function

    subroutine testBType (x)
        class (*), intent(in), target :: x

        type (bType), pointer :: b1

        b1 => x

        print *, b1
    end subroutine
end module

program fArg029a3_1
use m
    call testBType (replicateAnything (bType(2,4)))
end