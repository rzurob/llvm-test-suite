! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/26/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : final sub (step 2 of finalization process)
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
    type point
        real*4 :: x, y

        contains

        final :: invalidatePoint
    end type

    type lineSegment
        type (point) :: p1, p2
    end type

    contains

    subroutine invalidatePoint (p)
        type (point), intent(inout) :: p

        print *, 'point will be invalid after this call'
        p%x = sqrt(-1.0)
        p%y = sqrt(-1.0)
    end subroutine
end module

program ffinal005

use m

    type (point), pointer :: p1
    type (lineSegment), pointer :: line1

    allocate (line1, p1)

    deallocate (line1, p1)

end