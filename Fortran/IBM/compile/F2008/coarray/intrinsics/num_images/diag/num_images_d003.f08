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
!*  DESCRIPTION                : Num_images intrinsic requires CAF support.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program main
	integer :: num
	integer :: b

	num = num_images()
	num = fun0()

contains

	function fun0()
		integer :: fun0
		fun0 = num_images()
	end function
end
