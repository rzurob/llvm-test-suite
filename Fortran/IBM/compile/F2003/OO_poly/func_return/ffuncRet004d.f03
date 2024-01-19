! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 08/18/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : poly-function-return (type-compatibility for
!                               the pointer assignment)
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
    end type

    type, extends(base) :: child
        integer*4 :: id
    end type

    contains

    class (child) function produceChildPtr (c)
        class (child), intent(in) :: c

        pointer produceChildPtr

        allocate (produceChildPtr, source=c)
    end function
end module

program ffuncRet004d
use m
    type (base), pointer :: b

    b => produceChildPtr (child(1))
end
