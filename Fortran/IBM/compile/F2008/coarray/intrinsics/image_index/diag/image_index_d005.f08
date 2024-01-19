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
!*  DESCRIPTION                : Image_index requires the 2nd arg be a rank-1
!*				 integer array with size equal to the corank.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program main
	integer, save :: caf1[*], caf2[2,1:2,0:*]
	real, save :: caf3[10,0:*]
	logical, save :: caf4(2,2)[2,2,2,2,2,2,2,*]
	integer :: val

	val = image_index(caf1, [9])
	val = image_index(caf1, [1,2])
	val = image_index(caf1, [1,1,1])

	val = image_index(caf2, (/2,1,10/))
	val = image_index(caf2, (/2,1,0,1/))
	val = image_index(caf2, (/2,1/))
	val = image_index(caf2, (/6/))

	val = image_index(caf3, [1,1,1])
	val = image_index(caf3, [5])

	val = image_index(caf4, [1])
	val = image_index(caf4, [1,1,1,1,1,1,1])
end
