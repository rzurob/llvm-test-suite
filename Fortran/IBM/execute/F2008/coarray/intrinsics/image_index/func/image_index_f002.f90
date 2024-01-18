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
!*  DESCRIPTION                : Test image_index returns a value greater
!*                               than 1 and less than num_images() when a
!*				 valid 2nd arg is provided.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program main
	integer, save :: caf[0:*], caf2[1,*]
	integer num, val

	do i = 0, num_images() - 1
		num = num_images()
		val = image_index(caf, [i])
		call check_result(num, val)
		sync all
	end do


	do i = lcobound(caf2, 1), ucobound(caf2, 1)
		do j = lcobound(caf2, 2), ucobound(caf2, 2)
			val = image_index(caf2, [i,j])
			call check_result(num, val)
			sync all
		end do
	end do

end


subroutine check_result(n, v)
	integer :: n, v

	if (v < 1 .or. v > n) then
		print *, v, n
		error stop 11
	end if
end subroutine
