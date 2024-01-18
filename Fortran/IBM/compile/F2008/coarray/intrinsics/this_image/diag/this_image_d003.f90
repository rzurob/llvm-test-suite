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
!*  DESCRIPTION                : Invalid arguments test. First arg, if any,
!*                               must be of type coarray.
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

!!!!Valid
	print *, this_image(cafi2, i4)
	print *, this_image(cafi2, cafi4)
        print *, this_image(cafi4, cafi4[1])


!!!!Invalid
	print *, this_image(cafi1, i1)
	print *, this_image(cafi4, i8)
	print *, this_image(cafi8, i2)

	print *, this_image(cafi1, cafr4)
	print *, this_image(cafi4, cafx4[1])
	print *, this_image(cafi8, i8arr)
	print *, this_image(cafr8, cafx4(1,1,1))

	print *, this_image(cafr4, cx4)
	print *, this_image(cafr8, l8)
	print *, this_image(cafx4, r4)
	print *, this_image(cafx8, cx8)

	print *, this_image(cafl1, l1)
	print *, this_image(cafl2, l2)
	print *, this_image(cafl4, r8)
	print *, this_image(cafl8, l4)

	print *, this_image(cafch, ch)

end
