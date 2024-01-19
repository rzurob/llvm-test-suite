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
!*  DESCRIPTION                : Test Coarray execution statements with CAF syntax get flagged properly with CAF off.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program main
	integer, parameter :: n = 10
	integer :: a(n), b(n)

	a = (/ (i * 2, i = 1, n) /)
	a = a * 10

	do i = 1, n
		b[i] = a[i]
	end do
end
