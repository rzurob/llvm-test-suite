! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/15/2006
!*
!*  DESCRIPTION                : miscellaneous (defect 325347)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        character(:), pointer :: name => null()
    end type
end module

use m
    character(20), target :: c = 'xlftest'

    type(base) b1

    b1 = base (c)

    if (b1%name%len /= 20) error stop 1_4

    if (b1%name /= 'xlftest') error stop 2_4
end