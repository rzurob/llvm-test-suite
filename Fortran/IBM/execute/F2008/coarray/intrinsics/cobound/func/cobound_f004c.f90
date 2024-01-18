!234567890123456789012345678901234567890123456789012345678901234567890
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : cobound_f004c.f
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
!*  DESCRIPTION                : Test lcobound/ucobound with 2/3 arguments
!*                               produces consistent results across scopes. Also test the kind
!*                               type of these values.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module modFDC

contains

	subroutine sub1(size, n)
		integer :: n
		complex*8, save :: caf_mod(1:3,0:2,3)[n:4,n:*]
		integer :: size
		integer :: arr_lo4(size), arr_hi4(size), arr1(size), arr2(size)
		
		arr1 = lcobound(COARRAY=caf_mod, KIND=4)
		arr2 = ucobound(COARRAY=caf_mod, KIND=4)
		
		arr_lo4(1) = lcobound(caf_mod, 1, 4)
		arr_lo4(2) = lcobound(caf_mod, 2, 4)
		arr_hi4(1) = ucobound(caf_mod, 1, 4)
		arr_hi4(2) = ucobound(caf_mod, 2, 4)
		print *, arr_lo4, ":", arr1, ":", arr_hi4, ":", arr2
		sync all
	end subroutine
end module


program main
	use modFDC
	integer :: x, fun1, fun2
	integer, parameter :: size = 2
	
	call sub2(size, 0)
	x = fun1(size, 0)
	call sub1(size, 0)
	x = fun2(size, 0)
	
contains

	subroutine sub2(size, n)
		integer :: n
		character(3), save :: caf_int(1:3,0:2,3)[n:4,n:*]
		integer :: size
		integer(1) :: arr_lo1(size), arr_hi1(size), arr1(size), arr2(size)
		
		arr1 = lcobound(COARRAY=caf_int, KIND=1)
		arr2 = ucobound(COARRAY=caf_int, KIND=1)
		
		do i = 1, size
			arr_lo1(i) = lcobound(COARRAY=caf_int, DIM=i, KIND=1)
			arr_hi1(i) = ucobound(COARRAY=caf_int, DIM=i, KIND=1)
		end do
		print *, arr_lo1, ":", arr1, ":", arr_hi1, ":", arr2
		sync all
	end subroutine
	
end


integer function fun1(size, n)
	integer :: n
	logical(8), save :: caf_ext(1:3,0:2,3)[n:4,n:*]
	integer :: size
	integer(2) :: arr1(size), arr2(size), arr_lo2(size), arr_hi2(size)
	
	arr1 = lcobound(COARRAY=caf_ext, KIND=2)
	arr2 = ucobound(COARRAY=caf_ext, KIND=2)
	
	do i = 1, size
		arr_lo2(i) = lcobound(caf_ext, i, 2)
		arr_hi2(i) = ucobound(caf_ext, i, 2)	
	end do
	print *, arr_lo2, ":", arr1, ":", arr_hi2, ":", arr2
	sync all
	
	fun1 = 1
end function


integer function fun2(size, n)
	integer :: n
	logical(8), save :: caf_ext2(1:3,0:2,3)[n:4,n:*]
	integer :: size
	integer(2) :: arr1(size), arr2(size), arr_lo8(size), arr_hi8(size)
	
	arr1 = lcobound(COARRAY=caf_ext2, KIND=8)
	arr2 = ucobound(COARRAY=caf_ext2, KIND=8)
	
	do i = 1, size
		arr_lo8(i) = lcobound(caf_ext2, i, 8)
		arr_hi8(i) = ucobound(caf_ext2, i, 8)	
	end do
	print *, arr_lo8, ":", arr1, ":", arr_hi8, ":", arr2
	sync all
	
	fun2 = 1
end function
