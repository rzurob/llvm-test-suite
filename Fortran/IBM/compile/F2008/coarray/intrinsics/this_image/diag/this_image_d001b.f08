!234567890123456789012345678901234567890123456789012345678901234567890
!*  ===================================================================
!*
!*  DATE                       : July 2010
!*
!*  PRIMARY FUNCTIONS TESTED   : Coarray
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Invalid arguments test. Only accepts 0, 1, or 2
!*                               arguments, anything else is an error.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program main
	implicit none
	logical, save :: caf[*]
	integer :: w, x, y, z
	real*8 :: a, b, c

!#### 3 Args
	print *, this_image(x, y, z)

!#### 4 Args
	print *, this_image(caf, x, y, z)

!#### 5 Args
	print *, this_image(caf, z, b, c, x)

end