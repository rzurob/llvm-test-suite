!234567890123456789012345678901234567890123456789012345678901234567890
!*  ===================================================================
!*
!*  DATE                       : September 2010
!*
!*  PRIMARY FUNCTIONS TESTED   : Coarray
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Test lcobound/ucobound with an invalid
!*                               number of argments. These intrinsics only allow
!*                               1, 2, or 3 arguments.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program main

	integer*4, save :: caf[2,2,*], caf2[*]
	integer :: a, b, c, d

!#### 0 Args
	print *, lcobound()
	print *, ucobound()

!#### 4 Args
	print *, lcobound(caf, a, b, c)
	print *, ucobound(caf, a, b, c)

!#### 5 Args
	print *, lcobound(caf, a, b, c, d)
	print *, ucobound(caf, a, b, c, d)

end
