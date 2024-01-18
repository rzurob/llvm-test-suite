! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2011-07-21
!*  ORIGIN                     :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : defect 390697
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
    implicit none
    contains

    subroutine foo (x)
        real, dimension(*), codimension[0:*] :: x

        integer me, not_me

        me = this_image(x, 1)
        not_me = this_image()

        if (me+1 /= not_me) then
            print *, me+1, 'vs', not_me
            error stop 1
        end if
    end subroutine
end module

use m
    implicit none
    real, dimension(10), codimension[*], save :: x1

    call foo(x1)
end
