!234567890123456789012345678901234567890123456789012345678901234567890
!*  ===================================================================
!*
!*  DATE                       : July 2010
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Coarray
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : This_image requires CAF support.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program main
	integer :: num

	num = this_image()
	call sub1(caf)
end


subroutine sub1(caf)
	integer :: caf[*]
	integer, allocatable :: n(:)

	n = this_image(caf)
end subroutine
