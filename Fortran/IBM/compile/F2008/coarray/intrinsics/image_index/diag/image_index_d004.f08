!234567890123456789012345678901234567890123456789012345678901234567890
!*  ===================================================================
!*
!*  DATE                       : August 2010
!*
!*  PRIMARY FUNCTIONS TESTED   : Coarray
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Image_index requires CAF support.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program main
	integer :: caf
	integer :: val, y(1)

	val = image_index(caf, y)
end
