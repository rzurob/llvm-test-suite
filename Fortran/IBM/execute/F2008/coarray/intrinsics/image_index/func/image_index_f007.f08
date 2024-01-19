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
!*  DESCRIPTION                : Test image_index with a use associated
!*                               coarray.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module modFDC
	integer, save :: caf[2,2,*]
end module


program main
	use modFDC

	print *, image_index(caf, [1,1,1])
	sync all
	print *, image_index(caf, [1,1,2])
	sync all
	print *, image_index(caf, [1,2,1])
	sync all
	print *, image_index(caf, [1,2,2])
	sync all
	print *, image_index(caf, [2,1,1])
	sync all
	print *, image_index(caf, [2,1,2])
	sync all
	print *, image_index(caf, [2,2,1])
	sync all
	print *, image_index(caf, [2,2,2])
	sync all

end
