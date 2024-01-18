!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 02/08/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : final subroutine (finalization of the
!                               finalizable components of an array; test the
!                               step 2 of the finalization process; scalar
!                               components)
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
        real(4) :: x, y

        contains

        final :: invalidatePoint
    end type

    type lineSegment
        type (point) :: p1, p2

        contains

        final :: nullifyLine
    end type

    contains

    subroutine invalidatePoint (p)
        type (point), intent(inout) :: p

        print *, 'point will be invalid after this call'
        p%x = sqrt(-1.0)
        p%y = sqrt(-1.0)
    end subroutine

    subroutine nullifyLine (l)
        type (lineSegment), intent(inout) :: l(:)

        print *, 'in the finalization for lineSegment'
    end subroutine
end module

program ffinal005a1
use m
    type (lineSegment), pointer :: line1(:)

    allocate (line1(10))

    line1 = lineSegment (p1 = point(0, 0), p2 = point(1, 1))

    print *, 'test'

    deallocate (line1) !also finalizes p1 and p2 for each line1 element

    print *, 'end'
end
