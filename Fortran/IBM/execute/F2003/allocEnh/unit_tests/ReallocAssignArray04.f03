! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : June 5, 2006
!*
!*  DESCRIPTION                : Testing reallocation on assignment
!*                               with a variable as the right-hand
!*                               side of the assignment.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

byte, allocatable :: a(:)
byte b(5)
b = (/(i,i=1,5)/)
a = b
if (any(shape(a) /= (/5/))) error stop 1
if (any(a /= (/1,2,3,4,5/))) error stop 2
end