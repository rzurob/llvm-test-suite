!234567890123456789012345678901234567890123456789012345678901234567890
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : critical_f009.f
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
!*  DESCRIPTION                : Test critical construct containing a call to
!*                               a module procedure that uses local coarrays.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module modMSTRKRFT

contains
	subroutine sub0(noi)
		integer(2), save :: caf(10)[*]
		integer :: position, noi, var, hold
		integer, parameter :: offset = 100
	
		position = -1
		caf[1] = offset
		sync all
	
		critical
			position = caf(1)[1]
			call sleep_(1)
			caf[1] = position - 1
		end critical

		sync all

		if ( (position > offset) .or. (position < (offset - noi + 1)) ) then
			print *, "actual", position
			print *, offset, noi
			error stop 13
		end if
		
		var = 99 - noi + 1
		if ( any(caf[1] .ne. [(var, i = 1, 10)]) ) then
			print *, "actual", caf[1]
			print *, "expected", var
			error stop 14
		end if
		
	end subroutine
end module


program main
	use modMSTRKRFT
	integer :: num
	
	call sub0(num_images())
end
