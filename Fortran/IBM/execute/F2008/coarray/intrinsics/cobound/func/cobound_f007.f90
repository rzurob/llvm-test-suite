!234567890123456789012345678901234567890123456789012345678901234567890
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : cobound_f007.f
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
!*  DESCRIPTION                : Test lcobound/ucobound with host associated
!*                               coarray's.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program main

	integer, save :: caf(10)[2,*], caf2[11:*]
	integer :: alo(1), ahi(1)
	integer(4) :: al4(1), ah4(1)

	call sub1()


	alo = lcobound(caf2)
	ahi = ucobound(caf2)
	print *, alo, ":", ahi
	sync all
	
	alo(1) = lcobound(caf2, 1)
	ahi(1) = ucobound(caf2, 1)
	print *, alo, ":", ahi
	sync all
	
	al4 = lcobound(COARRAY=caf2, KIND=4)
	ah4 = ucobound(COARRAY=caf2, KIND=4)
	print *, al4, ":", ah4
	sync all
	
contains

	subroutine sub1()
		integer :: arr1(2), arr2(2)
		integer*1 :: a1(2)
		integer*8 :: a2(2)
	
		arr1 = 0
		arr2 = 0
		arr1 = lcobound(caf)
		arr2 = ucobound(caf)
		print *, arr1, ":", arr2
		sync all
		
		
		arr1 = 0
		arr2 = 0
		do i = 1, 2
			arr1(i) = lcobound(caf, i)
			arr2(i) = ucobound(caf, i)
		end do
		print *, arr1, ":", arr2
		sync all
		
		
		do i = 1, 2
			a1(i) = lcobound(caf, i, kind(a1))
			a2(i) = ucobound(caf, i, kind(a2))
		end do
		print *, a1, ":", a2
		sync all
	end subroutine
	
end
