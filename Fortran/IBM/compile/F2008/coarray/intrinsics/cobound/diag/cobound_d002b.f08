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
!*  DESCRIPTION                : Mix lcobound/ucobound intrinsic calls
!*                               with variable declarations/statements of
!*                               the same name.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program main

	integer, save :: caf[*]
	integer :: a, b(1), lcobound

	a = lcobound
	b = ucobound

	print *, lcobound(caf)
	print *, ucobound(caf)

	print *, lcobound(caf, 1)
	print *, ucobound(caf, 1)

	print *, ucobound(COARRAY=caf, DIM=1, KIND=1)
	print *, ucobound(COARRAY=caf, KIND=8, DIM=1)

end