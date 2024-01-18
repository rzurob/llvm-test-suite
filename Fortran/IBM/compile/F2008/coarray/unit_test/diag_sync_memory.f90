!234567890123456789012345678901234567890123456789012345678901234567890
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : diag_sync_mem.f
!*
!*  PROGRAMMER                 : Francesco Cassullo
!*  DATE                       : December 2010
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Coarray
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Test Sync Memory gets flagged properly when CAF is off.
!*
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program main
	integer :: arr(10)
	
	call sub1(1)
	call sub1(2)
	
end


subroutine sub1(x)
	integer :: x, arr(6)

	arr = 0
	if (x == 1) then
		sync memory
		arr = x
	else
		sync memory
		arr(x) = x
	end if
end subroutine