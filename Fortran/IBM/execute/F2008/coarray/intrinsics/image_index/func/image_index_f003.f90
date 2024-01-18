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
!*  DESCRIPTION                : Test image_index results with coarrays
!*                               having a corank of 1.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program main
	integer, save :: caf1[*]

	print *, image_index(caf1, [1])
	sync all
	print *, image_index(caf1, [2])
	sync all
	print *, image_index(caf1, [3])
	sync all
	print *, image_index(caf1, [4])
	sync all
	print *, image_index(caf1, [5])
	sync all
	print *, image_index(caf1, [6])

	sync all
	call sub1()
end


subroutine sub1()
	integer, save :: caf2[0:*]

	if (this_image() == 1) then
        	print *, ""
	end if
	sync all

	print *, image_index(caf2, [0])
	sync all
	print *, image_index(caf2, [1])
        sync all
	print *, image_index(caf2, [2])
        sync all
	print *, image_index(caf2, [3])
        sync all
	print *, image_index(caf2, [4])
        sync all
	print *, image_index(caf2, [5])

end subroutine
