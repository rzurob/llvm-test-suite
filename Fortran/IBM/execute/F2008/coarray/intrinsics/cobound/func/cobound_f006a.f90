!234567890123456789012345678901234567890123456789012345678901234567890
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : cobound_f006a.f
!*
!*  PROGRAMMER                 : Francesco Cassullo
!*  DATE                       : September 2010
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Coarray
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Test lcobound/ucobound with 1 argument
!*                               using a use associated coarray.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module modFDC
	real, save :: caf(3,3,3)[0:2,0:0,0:1,0:*]
	integer, parameter :: n = 4
end module


program main

	use modFDC
	integer :: arr1(n), arr2(n)

	arr1 = lcobound(caf)
	arr2 = ucobound(caf)
	
	if ( any(arr1 .ne. [0,0,0,0]) ) then
		print *, arr1
		error stop 11
	end if
	if ( any(arr2 .ne. [2,0,1,1]) ) then
		print *, arr2
		error stop 12
	end if
	sync all
	
end
