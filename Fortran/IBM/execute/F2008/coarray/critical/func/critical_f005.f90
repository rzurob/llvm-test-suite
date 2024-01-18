!234567890123456789012345678901234567890123456789012345678901234567890
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : critical_f005.f
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
!*  DESCRIPTION                : Test critical construct with global logical coarrays.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module nwl
	implicit none
	logical*1, save :: caf(550)[0:1,0:*]
end module


program main
	integer :: res, f1
	
	res = f1(this_image())
end


integer function f1(n)
	use nwl
	integer :: i
	logical :: lo
	
	lo = .true.
	sync all
	
	do i = 1, 2100
		critical
			if (this_image() == 1) then
				caf(:) = lo
			else
				if (caf(1) .neqv. caf(550)) then
					print *, caf(1), caf(550)
					error stop 15
				end if
				
				if (caf(15) .neqv. caf(77)) then
					print *, caf(15), caf(77)
					error stop 16
				end if
			end if
		end critical
	end do
	
	f1 = 1
end function
