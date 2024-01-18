!234567890123456789012345678901234567890123456789012345678901234567890
!*  ===================================================================
!*
!*  DATE                       : September 2010
!*
!*  PRIMARY FUNCTIONS TESTED   : Coarray
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Invalid argument testing of the first
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
	print *, lcobound(i1)
	print *, lcobound(i2)
	print *, lcobound(i4)
	print *, lcobound(i8)

	print *, lcobound(r4)
	print *, lcobound(r8)
	print *, lcobound(cx4)
	print *, lcobound(cx8)

	print *, lcobound(l1)
	print *, lcobound(l2)
	print *, lcobound(l4)
	print *, lcobound(l8)
	print *, lcobound(ch)

	print *, lcobound(cafi4[1])			!valid
	print *, lcobound(cafx4[1])			!valid
	print *, lcobound(cafl8(9)[1])		        !valid


!### ucobound
	print *, ucobound(i1)
	print *, ucobound(i2)
	print *, ucobound(i4)
	print *, ucobound(i8)

	print *, ucobound(r4)
	print *, ucobound(r8)
	print *, ucobound(cx4)
	print *, ucobound(cx8)

	print *, ucobound(l1)
	print *, ucobound(l2)
	print *, ucobound(l4)
	print *, ucobound(l8)
	print *, ucobound(ch)

	print *, ucobound(cafl1[1])			!valid
	print *, ucobound(cafi8[1])			!valid
	print *, ucobound(cafx4(1,1,1)[1])		!valid

end
