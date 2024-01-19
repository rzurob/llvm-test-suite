!234567890123456789012345678901234567890123456789012345678901234567890
!*  ===================================================================
!*
!*  DATE                       : December 2010
!*
!*  PRIMARY FUNCTIONS TESTED   : Coarray
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Test Sync All gets flagged properly when CAF is off.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program main
	integer :: arr(10)

	arr = 0
	do i = 1, 10
		arr(i) = arr(i) + i
	end do

	print *, arr(1:5)
	sync all
	print *, arr(6:10)
end
