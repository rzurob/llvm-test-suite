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
!*  TEST CASE TITLE            : ReallocAssignArrayCharacter09
!*
!*  PROGRAMMER                 : Rob James
!*  DATE                       : June 9, 2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*  DESCRIPTION                : Testing reallocation on assignment,
!*                               with an array of characters on the
!*                               left-hand side of the assignment.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

character(5) :: x(5)
call sub(x)
contains
  subroutine sub(b)
    character(5), allocatable :: a(:)
    character(*) :: b(:)
    b = (/(repeat(char(i),5),i=69,73)/)
    a = b
    if (.not. allocated(a)) stop 1
    if (any(shape(a) /= shape(b))) stop 2
    if (lbound(a,1) /= 1) stop 3
    if (ubound(a,1) /= 5) stop 4
    if (any(a /= (/(repeat(char(i),5),i=69,73)/))) stop 5
  end subroutine
end
