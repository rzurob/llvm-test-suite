!234567890123456789012345678901234567890123456789012345678901234567890
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : cobound_d004.f
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
!*  DESCRIPTION                : Invalid argument testing of the second 
!*                               argument of lcobound/ucobound.
!*                            
!234567890123456789012345678901234567890123456789012345678901234567890

program main
	
	integer*1, save :: i1, cafi1[*]
	integer*2, save :: i2, cafi2[*]
	integer*4, save :: i4, cafi4[*]
	integer*8, save :: i8, cafi8(10)[*]
	real*8, save :: r8, cafr8(1,2)[*]
	real*4, save :: r4, cafr4[*]
	complex(4), save :: cx4, cafx4(3,3,3)[*]
	complex(8), save :: cx8, cafx8[*]
	character(3), save :: ch, cafch[*]
	logical*1, save :: l1, cafl1[*]
	logical*2, save :: l2, cafl2[*]
	logical*4, save :: l4, cafl4[*]
	logical*8, save :: l8, cafl8(55)[*]

	
!### lcobound
	print *, lcobound(cafi1, i1)			!valid
	print *, lcobound(cafi2, i2)			!valid
	print *, lcobound(cafi4, i4)			!valid
	print *, lcobound(cafi8, i8)			!valid

	print *, lcobound(cafr4, r4)
	print *, lcobound(cafr8, r8)
	print *, lcobound(cafx4, cx4)
	print *, lcobound(cafx8, cx8)

	print *, lcobound(cafl1, l1)
	print *, lcobound(cafl2, l2)
	print *, lcobound(cafl4, l4)
	print *, lcobound(cafl8, l8)
	print *, lcobound(cafch, ch)
	
	print *, lcobound(cafi4[1], cafi4[1])	        !valid
	
	
!### ucobound
	print *, ucobound(cafi1, i1)			!valid
	print *, ucobound(cafi2, i2)			!valid
	print *, ucobound(cafi4, i4)			!valid
	print *, ucobound(cafi8, i8)			!valid

	print *, ucobound(cafr4, r4)
	print *, ucobound(cafr8, r8)
	print *, ucobound(cafx4, cx4)
	print *, ucobound(cafx8, cx8)

	print *, ucobound(cafl1, l1)
	print *, ucobound(cafl2, l2)
	print *, ucobound(cafl4, l4)
	print *, ucobound(cafl8, l8)
	print *, ucobound(cafch, ch)
	
	print *, ucobound(cafi8(1)[1], cafi8(1)[1])	!valid
	
end
