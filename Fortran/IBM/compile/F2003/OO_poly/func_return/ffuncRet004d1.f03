! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 12/03/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : function return (diagnostic that test IO can't
!                               be invoked for polymorphic function return
!                               results without DTIO)
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
        integer i
    end type

    type, extends(base) :: child
        real r
    end type

    contains

    class (base) function genData (i, r)
        integer, intent(in) :: i
        real, intent(in), optional :: r

        pointer genData

        if (present(r)) then
            allocate (genData, source=child(i, r))
        else
            allocate (genData, source=base(i))
        end if
    end function
end module

program ffuncRet004d1
use m
    print *, genData (100, 1.9)     !<-- illegal
end
