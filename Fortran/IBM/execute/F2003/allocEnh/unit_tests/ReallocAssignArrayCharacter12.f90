!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : June 9, 2006
!*
!*  DESCRIPTION                : Testing reallocation on assignment,
!*                               with an array of characters on the
!*                               left-hand side of the assignment.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

character(:), allocatable :: a(:)
character(5) :: b(5)
b = (/(char(i),i=1,5)/)
a = b
if (.not. allocated(a)) stop 1
if (any(shape(a) /= shape(b))) stop 2
if (lbound(a,1) /= 1) stop 3
if (ubound(a,1) /= 5) stop 4
if (any(a /= (/(char(i),i=1,5)/))) stop 5
if (len(a) /= 5) stop 6
end
