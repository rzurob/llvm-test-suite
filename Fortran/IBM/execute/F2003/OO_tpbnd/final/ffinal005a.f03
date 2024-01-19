! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/07/2005
!*
!*  DESCRIPTION                : final subroutine (finalizable scalar components
!                               finalized in step 2)
!*
!*  KEYWORD(S)                 :
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


program ffinal005a
use m

    type (lineSegment), pointer :: line1(:)

    allocate (line1(10))

    line1 = lineSegment (p1 = point(0, 0), p2 = point(1, 1))

    print *, 'test'
    deallocate (line1) !should invoke finals for p1 and p2 for each line1 element
end
