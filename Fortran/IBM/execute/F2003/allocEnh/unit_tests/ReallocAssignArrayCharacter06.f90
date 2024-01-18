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
!*  TEST CASE TITLE            : ReallocAssignArrayCharacter06
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

character(5), allocatable :: a(:)
character(:), pointer :: b(:)
allocate(character(5) :: b(5))
allocate(a(5))
b = (/(char(i),i=1,5)/)
a = b
if (.not. allocated(a)) stop 1
if (any(shape(a) /= shape(b))) stop 2
if (lbound(a,1) /= 1) stop 3
if (ubound(a,1) /= 5) stop 4
if (any(a /= (/char(1),char(2),char(3),char(4),char(5)/))) stop 5
end
