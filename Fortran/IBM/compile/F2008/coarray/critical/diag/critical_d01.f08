!234567890123456789012345678901234567890123456789012345678901234567890
!*  ===================================================================
!*
!*  DATE                       : January 2011
!*
!*  PRIMARY FUNCTIONS TESTED   : Coarray
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Test the Critical construct gets flagged when CAF is off.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program fdc
	integer :: arr(10)

	arr = 0
	do i = 1, 10
		critical
			arr(i) = arr(i) + i
		end critical

		arr(i) = (-1) * arr(i)
	end do

end