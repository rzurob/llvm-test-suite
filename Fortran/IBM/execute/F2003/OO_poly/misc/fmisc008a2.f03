! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/13/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : miscellaneous items (defect 292722)
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

program fmisc008a2
use m
    type (base), allocatable :: b1(:)

    allocate (b1(2:5))

    b1%id = (/2,3,4,5/)

    associate (x => b1(3:)%id)
        if (any(x(1:2) /= (/3,4/))) error stop 1_4

        associate (x1 => x(2))
            if (x1 /= 4)  error stop 2_4
        end associate
    end associate

end