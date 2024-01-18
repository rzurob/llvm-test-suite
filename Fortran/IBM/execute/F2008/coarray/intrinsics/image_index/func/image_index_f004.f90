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
!*  DESCRIPTION                : Test image_index results with coarrays
!*                               having a corank of 2.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program main
	integer, save :: caf1[4,0:*]
	integer :: arr(4,2,8), arr2(4,2)
	arr = -1
	arr2 = -1

	do k = 1, 8
                if (this_image() == k) then
                        do i = 1, 4
                                do j = 1, 2
                                        arr(i,j,k) = image_index(caf1, [i,j-1])
                                end do
                        end do
                end if
        end do

	do k = 1, 8
		sync all
		if (this_image() == k) then
			print *, arr(1,:,k), ":", arr(2,:,k), ":", arr(3,:,k), ":", arr(4,:,k)
		end if
		sync all
	end do

end
