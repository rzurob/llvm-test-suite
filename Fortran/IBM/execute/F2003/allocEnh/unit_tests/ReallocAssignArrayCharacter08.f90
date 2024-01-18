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

character(5), allocatable :: a(:)
character(:), allocatable :: b(:)
allocate(character(5) :: b(5:9))
b = (/(repeat(char(i),5),i=1,5)/)
a = b
if (.not. allocated(a)) stop 1
if (any(shape(a) /= shape(b))) stop 2
if (lbound(a,1) /= 5) stop 3
if (ubound(a,1) /= 9) stop 4
if (any(a /= (/(repeat(char(i),5),i=1,5)/))) stop 5
end
