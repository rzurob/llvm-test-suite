! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/21/2005
!*
!*  DESCRIPTION                : miscellaneous (bounds in the associate)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    contains

    integer function abc()
        allocatable abc(:)

        allocate (abc(3:5))
        abc =(/3,4,5/)
    end function
end module

program fmisc029a1
use m
    associate (x => abc())
        if ((lbound(x,1) /= 1) .or. (ubound(x, 1) /= 3))  error stop 1_4

        if (any (x /= (/3,4,5/))) error stop 2_4
    end associate
end
