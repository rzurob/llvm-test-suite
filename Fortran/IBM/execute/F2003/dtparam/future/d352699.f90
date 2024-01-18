! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/30/2009
!*
!*  DESCRIPTION                : miscellanous (352699)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type X (n)
        integer, len :: n
        real :: data(n)
    end type

    contains

    subroutine printX1X2 (x1, x2)
        type(X(*)), intent(in) :: x1, x2

        namelist /nm1/ x1, x2

        write (*, nml=nm1)
    end subroutine
end module

use m
    type(X(1)) x1
    type(X(2)) x2

    x1%data = 1.0
    x2%data = [2.0, 3.0]


    call printX1X2 (x1, x2)
end
