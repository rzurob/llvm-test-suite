!234567890123456789012345678901234567890123456789012345678901234567890
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : cobound_d005.f
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
!*  DESCRIPTION                : Invalid argument testing of the KIND 
!*                               argument of lcobound/ucobound. This can
!*                               be the 2nd or 3rd argument depending.
!*                            
!234567890123456789012345678901234567890123456789012345678901234567890

program main
	
	integer, save :: caf[*]
	integer*1, parameter :: i1 = 1
	integer*2, parameter :: i2 = 2
	integer*4, parameter :: i4 = 4
	integer*8, parameter :: i8 = 8
	real*8, parameter :: r8 = 8.0
	real*4, parameter :: r4 = 4.0
	complex(4), parameter :: cx4 = (4.0, 0.0)
	complex(8), parameter :: cx8 = (8.0, 0.0)
	integer :: func1
	
!### lcobound
	print *, lcobound(caf, i1, i1)
	print *, lcobound(caf, i1, i2)
	print *, lcobound(caf, i1, i4)
	print *, lcobound(caf, i1, i8)
	print *, lcobound(caf, i1, kind(i1))

	print *, lcobound(caf, i1, r4)
	print *, lcobound(caf, i1, r8)
	print *, lcobound(caf, i1, cx4)
	print *, lcobound(caf, i1, cx8)
	
	print *, lcobound(caf, i1, caf)
	print *, lcobound(caf, i1, caf[1])
	print *, lcobound(caf, i1, func1())
	
	
!### ucobound	
	print *, ucobound(caf, i1, i1)
	print *, ucobound(caf, i1, i2)
	print *, ucobound(caf, i1, i4)
	print *, ucobound(caf, i1, i8)
	print *, ucobound(caf, i1, kind(i8))

	print *, ucobound(caf, i1, r4)
	print *, ucobound(caf, i1, r8)
	print *, ucobound(caf, i1, cx4)
	print *, ucobound(caf, i1, cx8)
	
	print *, ucobound(caf, i1, caf)
	print *, ucobound(caf, i1, caf[1])
	
end


integer function func1()
	func1 = 8
end function