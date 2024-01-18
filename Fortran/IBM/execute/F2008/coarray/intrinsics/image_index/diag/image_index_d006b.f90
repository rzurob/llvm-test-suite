!234567890123456789012345678901234567890123456789012345678901234567890
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : image_index_d006b.f
!*
!*  PROGRAMMER                 : Francesco Cassullo
!*  DATE                       : August 2010
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Coarray
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Mix image_index() intrinsic calls with variable
!*                              statements and declarations of the same name.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program main
	integer, save :: caf[*]
	real, save :: caf2(1,2,3)[0:*]
	integer :: x, y, z, image_index
	integer :: w(1)

	image_index = 1
	do i = 1, image_index
		print *, image_index(caf, w)
	end do
	
	if (this_image() == 1) then
		print *, image_index(caf2, [1])
		print *, image_index(caf2, [2])
		print *, image_index(caf2, [3])
	end if

	print *, image_index
end
