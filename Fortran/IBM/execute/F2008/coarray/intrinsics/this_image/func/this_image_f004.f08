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
!*  DESCRIPTION                : This_image() should produce the same result as
!*				 image_index(caf, this_image(caf)) for all images.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program main

	integer, save :: icaf1[3:5,*], icaf2[*]
	real, save :: rcaf1[2,9:10,7:*], rcaf2(10)[*]
	integer :: num
	integer, allocatable :: arr(:)


	num = this_image()
	arr = this_image(icaf1)
	if ( num /= image_index(icaf1, arr) ) then
		print *, arr
		print *, num
		error stop 11
	end if


	sync all
	num = this_image()
        if ( num /= image_index(icaf2, this_image(icaf2)) ) then
                print *, this_image(icaf2)
                print *, num
                error stop 12
        end if


        sync all
        num = this_image()
        if ( num /= image_index(rcaf1, this_image(rcaf1)) ) then
                print *, this_image(rcaf1)
                print *, num
                error stop 13
        end if


        sync all
        num = this_image()
        if ( num /= image_index(rcaf2, this_image(rcaf2)) ) then
                print *, this_image(rcaf2)
                print *, num
                error stop 14
        end if

end
