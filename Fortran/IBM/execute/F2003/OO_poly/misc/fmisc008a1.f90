! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/23/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : miscellaneous items (defect 288894)
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
        integer(4) :: id = -1
    end type

end module

program fmisc008a1
use m
    type (base), allocatable :: b1(:)

    allocate (b1(2:5))

    b1%id = (/2,3,4,5/)

    associate (x => b1(3:)%id)
        if (size (x) /= 3) error stop 1_4

        if (any (x /= (/3,4,5/))) error stop 2_4
    end associate

end
