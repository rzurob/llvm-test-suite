!234567890123456789012345678901234567890123456789012345678901234567890
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : cobound_f005b.f
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
!*                               with a dummy argument coarray.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program main

	integer(1), save :: caf[0:1,0:0,0:1,0:*]
	integer, parameter :: n = 4
	integer, allocatable :: arr1(:), arr2(:)

	allocate(arr1(n), arr2(n))
	do i = 1, n
		arr1(i) = lcobound(caf, i)
		arr2(i) = ucobound(caf, i)
	end do
	
	print *, arr1, ":", arr2
	deallocate(arr1, arr2)
	sync all
	
	call sub1(caf, n)

contains

	subroutine sub1(coary, n)

		integer(1), save :: coary[2,1,2,*]
		integer :: n
		integer :: arr1(n), arr2(n)
		
		do i = 1, n
			arr1(i) = lcobound(COARRAY=coary, DIM=i)
			arr2(i) = ucobound(COARRAY=coary, DIM=i)
		end do
		
		print *, arr1, ":", arr2
		sync all
		
	end subroutine
	
end