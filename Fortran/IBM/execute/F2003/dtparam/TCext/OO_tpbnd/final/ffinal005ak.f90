! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : ffinal005ak
!*
!*  DATE                       : 2007-10-31 (original: 02/07/2005)
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
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
    type point (kpoint_1) ! kpoint_1=4
       integer, kind :: kpoint_1
        real(kpoint_1) :: x, y

        contains

        final :: invalidatePoint
    end type

    type lineSegment (klineSegment_1) ! klineSegment_1=4
       integer, kind :: klineSegment_1
        type (point(klineSegment_1)) :: p1, p2 ! tcx: (klineSegment_1)
    end type

    contains

    subroutine invalidatePoint (p)
        type (point(4)), intent(inout) :: p ! tcx: (4)

        print *, 'point will be invalid after this call'
        p%x = sqrt(-1.0)
        p%y = sqrt(-1.0)
    end subroutine
end module


program ffinal005ak
use m

    type (lineSegment(4)), pointer :: line1(:) ! tcx: (4)

    allocate (line1(10))

    line1 = lineSegment(4) (p1 = point(4)(0, 0), p2 = point(4)(1, 1)) ! tcx: (4) ! tcx: (4) ! tcx: (4)

    print *, 'test'
    deallocate (line1) !should invoke finals for p1 and p2 for each line1 element
end


! Extensions to introduce derived type parameters:
! type: point - added parameters (kpoint_1) to invoke with (4) / declare with (4) - 4 changes
! type: lineSegment - added parameters (klineSegment_1) to invoke with (4) / declare with (4) - 2 changes
