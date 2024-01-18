!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/31/2007
!*
!*  DESCRIPTION                : miscellaneous (defect 337441)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type X (k)
        integer, kind :: k = 4

        integer(k) i

        contains

        procedure :: getObj
    end type

    type Y (n)
        integer, len :: n = 10

        integer val(n)

        contains

        procedure :: getObj => getYObj
    end type

    contains

    type(X) function getObj (x1)
        class(X), intent(in) :: x1

        getObj%i = x1%i
    end function

    type(Y) function getYObj (y1)
        class(Y(*)), intent(in) :: y1

        if (y1%n < getYObj%n) stop 10

        getYObj%val = y1%val(:getYObj%n)
    end function
end module

use m
    type(x) x1
    type(Y(:)), allocatable :: y1

    x1%i = 100

    allocate (Y(20) :: y1)

    y1%val= [(i, i=1,20)]

    print *, x1%getObj()
    print *, y1%getObj()
    end
