!234567890123456789012345678901234567890123456789012345678901234567890
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : image_index_f005.f
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
!*  DESCRIPTION                : Test image_index results with coarrays
!*                               having a corank greater than 2.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program main
	integer, save :: caf3[1,4,*]
	integer :: arr(12), count
	arr = -1
	count = 1


	do i = 1, 1
		do j = 1, 4
        		do k = 1, 3
                		arr(count) = image_index(caf3, [i,j,k])
				count = count + 1
			end do
		end do
	end do

	sync all
	print *, arr
	sync all
	call sub0()

end


subroutine sub0()
	real, save :: caf4[1,2,2,*]
	integer :: arr(12), count
        arr = -1
        count = 1

	do i = 1, 2
                do j = 1, 2
                        do k = 1, 3
                                arr(count) = image_index(caf4, [1,i,j,k])
                                count = count + 1
                        end do
                end do
	end do

        sync all
        print *, arr
end subroutine
