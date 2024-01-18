!234567890123456789012345678901234567890123456789012345678901234567890
!*  ===================================================================
!*
!*  DATE                       : August 2010
!*
!*  PRIMARY FUNCTIONS TESTED   : Coarray
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Return type test. Image_index returns
!*                               a default scalar integer.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program main

	integer*1, save :: i1, cafi1[*]
	integer*2, save :: i2, cafi2[*]
	integer*4, save :: i4(1), i4arr(10), cafi4[*]
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


	i1 = image_index(cafi1, i4)
	i2 = image_index(cafi2, i4)
	i4arr(1) = image_index(cafi4, i4)
	i8 = image_index(cafi8, i4)

	r4 = image_index(cafi8, i4)
	r8 = image_index(cafr4, i4)
	cx4 = image_index(cafr8, i4)
	cx8 = image_index(cafx4, i4)
	ch = image_index(cafch, i4)

	l1 = image_index(cafl1, i4)
	l2 = image_index(cafl2, i4)
	l4 = image_index(cafl4, i4)
	l8 = image_index(cafl8, i4)

	i4arr = image_index(cafl8, i4)
	cafi4 = image_index(cafl1, i4)
	cafi4[1] = image_index(cafr4, i4)

end

