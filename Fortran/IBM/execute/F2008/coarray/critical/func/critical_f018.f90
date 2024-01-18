!234567890123456789012345678901234567890123456789012345678901234567890
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : critical_f018.f
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
!*  DESCRIPTION                : Verify the value of this_image() called within
!*                               a critical construct.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program main
	integer, save :: c1(5000)[10:*], c2(5000)[1,2,1,2,1,2,2,*]
	integer :: me
	
	me = this_image()
	sync all
	
	do i = 1, 10000
		first: critical
			if (this_image() == 1) then
				c1(:) = this_image()
				c2(:) = me
			else
				if ( c1(1) /= c1(5000) ) then
					print *, c1(1), c1(5000)
					error stop 15
				end if
				
				if ( c2(2) /= c2(4999) ) then
					print *, c2(1), c2(5000)
					error stop 16
				end if
			end if
		end critical first
	end do
end
