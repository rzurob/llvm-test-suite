! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : June 5, 2006
!*
!*  DESCRIPTION                : Testing reallocation on assignment,
!*                               with an array constructor as the
!*                               right-hand side of the assignment.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

complex(16), allocatable :: a(:)
a = (/(1.0_16,2.0_16), (3.0_16,4.0_16), (5.0_16,6.0_16)/)
if (any(shape(a) /= (/3/))) error stop 1
if (any(a /= (/(1.0_16,2.0_16), (3.0_16,4.0_16), (5.0_16,6.0_16)/))) error stop 2
if (lbound(a,1) /= 1) error stop 3
if (ubound(a,1) /= 3) error stop 4
end