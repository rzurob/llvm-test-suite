! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 08/08/2005
!*
!*  DESCRIPTION                : miscellaneous items (defect 307074)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m

    contains

    integer function x1()
        pointer x1

        allocate (x1)
        x1 = 10
    end function
end module

use m
    procedure (x1) y1

    associate (x => y1)     !<-- illegal

    end associate

    end