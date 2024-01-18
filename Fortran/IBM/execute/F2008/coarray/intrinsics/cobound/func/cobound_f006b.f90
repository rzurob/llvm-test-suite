!234567890123456789012345678901234567890123456789012345678901234567890
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : cobound_f006b.f
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
!*  DESCRIPTION                : Test lcobound/ucobound with 2 arguments
!*                               using a use associated coarray.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module modFDC
	logical, save :: caf[2,2,3:4,2,*]
end module


program main
	integer, parameter :: n = 5
	
	call sub1(n)
end


subroutine sub1(n)
	use modFDC
	integer :: n
	integer :: arr1(n), arr2(n)

	do i = 1, n
		arr1(i) = lcobound(caf, i)
		arr2(i) = ucobound(caf, i)
	end do

	print *, arr1, ":", arr2
	sync all
end subroutine
