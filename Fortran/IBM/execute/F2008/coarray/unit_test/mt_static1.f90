!234567890123456789012345678901234567890123456789012345678901234567890
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : mt_static1.f
!*
!*  PROGRAMMER                 : Francesco Cassullo
!*  DATE                       : December 2010
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Coarray
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Test memory of coarray data is properly 
!*                               stored across external units.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program main
	implicit none
	
	if (this_image() == 1) then
		call sub1()
	else
		call sub2()
	end if
end


subroutine sub1()
	integer, save :: caf[*]
	print *, "begin sub1:", caf 		! should be "0", but is undefined
	caf[1] = this_image()
	sync all

	if (caf[1] /= this_image()) then	! should be "1"
		print *, caf[1]
		error stop 21
	end if
end subroutine


subroutine sub2()
	integer, save :: caf[*]
	print *, "begin sub2:", caf		! should be "0", but is undefined
	caf[1] = this_image()
	sync all	

	! should be "2" to "n", where n is number of images
	if (caf[1] < 2 .or. caf[1] > num_images()) then
		print *, caf[1]
		error stop 31
	end if
end subroutine

