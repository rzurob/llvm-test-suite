!234567890123456789012345678901234567890123456789012345678901234567890
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : critical_f006.f
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
!*  DESCRIPTION                : Test critical construct with global character coarrays.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module stp
	implicit none
	character(2), save :: caf(1000)[3,4,5,*]
end module


program main
	use stp
	character(2) :: num
	integer :: f
	
	if (num_images() < 99) then
		write(num, '(I2)') this_image()
		f = f1(num)
	end if	

contains

	integer function f1(ch)
		character(2) :: ch
	
		sync all
	
		do i = 1, 1000
			critical
				if (this_image() == 1) then
					caf(:) = ch
				else
					if ( caf(1) /= caf(1000) ) then
						print *, caf(1), caf(1000)
						error stop 15
					end if
				end if
			end critical
		end do
	
	end function
end
