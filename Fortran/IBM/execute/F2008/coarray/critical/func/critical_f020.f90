!234567890123456789012345678901234567890123456789012345678901234567890
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : critical_f020.f
!*
!*  PROGRAMMER                 : Francesco Cassullo
!*  DATE                       : January 2011
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Coarray
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Verify the value of image_index() called within
!*                               a critical construct.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program main
	integer*8, save :: c1(3)[1,2,*], c2[*], c3[*]
	integer :: me
	
	num = num_images()
	c1 = 0
	c2 = 0
	c3 = 0
	sync all

	critical
		do i = 1, num_images()
			c2[1] = c3 - image_index(c2, [i])
		end do
		
		call sub0(1,1,1)
	end critical
	
	sync all

	if ( c2[1] /= (-1 * num) ) then
		print *, c2[1]
		error stop 20
	end if
	
contains

	subroutine sub0(i,j,k)
		integer :: i,j,k
		integer*8 :: tmp(3)
		
		tmp = c1[i,j,k]
		c1(1)[i,j,k] = tmp(1) + image_index(c1, [i,j,k])
		c1(2)[i,j,k] = tmp(2) - image_index(c1, [i,j,k])
		c1(3)[i,j,k] = c1(1)[i,j,k] + c1(2)[i,j,k]
	end subroutine
end
