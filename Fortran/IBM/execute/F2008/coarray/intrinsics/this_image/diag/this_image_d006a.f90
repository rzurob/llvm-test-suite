!234567890123456789012345678901234567890123456789012345678901234567890
!*  ===================================================================
!*
!*  DATE                       : July 2010
!*
!*  PRIMARY FUNCTIONS TESTED   : Coarray
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Test out of bounds values for the 2nd arg
!*                               of this_image intrinsic.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program main
	integer*1, save :: i1, cafi1[*]
	integer*2, save :: i2, cafi2[*]
	integer*4, save :: i4, cafi4[*]
	integer*8, save :: i8, i8arr(10), cafi8(10)[*]
	real*8, save :: r8, cafr8(1,2)[*]
	real*4, save :: r4, cafr4[*]
	complex(4), save :: cx4, cafx4(3,3,3)[*]
	complex(8), save :: cx8, cafx8[*]
	character(3), save :: ch, cafch[*]
	logical*1, save :: l1, cafl1[*]
	logical*2, save :: l2, cafl2[*]
	logical*4, save :: l4, cafl4[*]
	logical*8, save :: l8, cafl8(55)[*]


	num = this_image(cafi2, 5)
	num = this_image(cafi8, 6)
	num = this_image(cafr8, 7)
	num = this_image(cafl4, 8)
	num = this_image(cafx4, 9)
	num = this_image(cafi1, 10)
	num = this_image(cafl2, 11)
	num = this_image(cafi4, 12)
	num = this_image(cafch, 13)
	num = this_image(cafr4, 14)
	num = this_image(cafl1, 15)
	num = this_image(cafx8, 16)
	num = this_image(cafl8, huge(i4))
end
