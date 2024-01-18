!234567890123456789012345678901234567890123456789012345678901234567890
!*  ===================================================================
!*
!*  DATE                       : August 2010
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Coarray
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Test image_index with this_image.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program main
        integer, save :: caf[0:*], caf2[2,*]

	sync all
	if (this_image() == 1) then
		print *, "1"
		if ( image_index(caf2, this_image(caf2)) /= image_index(caf2, [1,1]) ) then
			print *, image_index(caf2, this_image(caf2))
			error stop 11
		end if
	else if (this_image() == 2) then
		print *, "2"
		if ( image_index(caf2, this_image(caf2)) /= image_index(caf2, [2,1]) ) then
			print *, image_index(caf2, this_image(caf2))
                        error stop 12
                end if
	else if (this_image() == 3) then
		print *, "3"
		if ( image_index(caf2, this_image(caf2)) /= image_index(caf2, [1,2]) ) then
			print *, image_index(caf2, this_image(caf2))
                        error stop 13
                end if
	else if (this_image() == 4) then
		print *, "4"
		if ( image_index(caf2, this_image(caf2)) /= image_index(caf2, [2,2]) ) then
			print *, image_index(caf2, this_image(caf2))
                        error stop 14
                end if
	else if (this_image() == 5) then
		print *, "5"
		if ( image_index(caf2, this_image(caf2)) /= image_index(caf2, [1,3]) ) then
			print *, image_index(caf2, this_image(caf2))
                        error stop 15
                end if
	else if (this_image() == 6) then
		print *, "6"
		if ( image_index(caf2, this_image(caf2)) /= image_index(caf2, [2,3]) ) then
			print *, image_index(caf2, this_image(caf2))
                        error stop 16
                end if
	else if (this_image() == 7) then
		print *, "7"
		if ( image_index(caf2, this_image(caf2)) /= image_index(caf2, [1,4]) ) then
			print *, image_index(caf2, this_image(caf2))
                        error stop 17
                end if
	else if (this_image() == 8) then
		print *, "8"
		if ( image_index(caf2, this_image(caf2)) /= image_index(caf2, [2,4]) ) then
			print *, image_index(caf2, this_image(caf2))
                        error stop 18
                end if
	else
		print *, "Image count exceeds expected limit."
		error stop 50
	end if
end
