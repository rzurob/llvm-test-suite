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
!*  DESCRIPTION                : Invalid arguments test. Test mixing intrinsic calls
!*                               of this_image() with variables of the same name.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module modFDC
	logical, save :: caf[*]

end module


program main
	use modFDC
	implicit none

	integer :: num, x, y, z
	real*8 :: c

	num = this_image

	if (this_image(caf, 1, x) == 1) then
		print *, ""
	end if


	print *, this_image(caf, 99999)

	x = 1
	y = 2
	z = 3
	num = this_image(caf, x, y, z)
end
