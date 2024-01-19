! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/20/2007
!*
!*  DESCRIPTION                : miscellaneous (defect 327740)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
    end type
end module

use m
    type, extends(base) :: child
        integer i
    end type

    class(child) func1
    class(base) func2

    external :: func1, func2
end
