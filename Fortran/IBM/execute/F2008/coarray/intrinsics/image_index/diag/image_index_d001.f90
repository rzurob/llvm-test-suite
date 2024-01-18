!234567890123456789012345678901234567890123456789012345678901234567890
!*  ===================================================================
!*
!*  DATE                       : August 2010
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Coarray
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Invalid arguments test. First arg must
!*                               be a coarray object of any type.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program main
	integer*1, save :: i1(1), cafi1[*]
	integer*2, save :: i2(1), cafi2[*]
	integer*4, save :: i4(1), cafi4[*]
	integer*8, save :: i8(1), i8scal, cafi8(10)[*]
	real*8, save :: r8(1), cafr8(1,2)[*]
	real*4, save :: r4(1), cafr4[*]
	complex(4), save :: cx4(1), cafx4(3,3,3)[*]
	complex(8), save :: cx8(1), cafx8[*]
	character(3), save :: ch(1), cafch[*]
	logical*1, save :: l1(1), cafl1[*]
	logical*2, save :: l2(1), cafl2[*]
	logical*4, save :: l4(1), cafl4[*]
	logical*8, save :: l8(1), l8scal, cafl8(55)[*]


!!!!Valid
	print *, image_index(cafi1, i4)
	print *, image_index(cafi1, i4)
	print *, image_index(cafi2, i4)
	print *, image_index(cafi4, i4)
	print *, image_index(cafi8, i4)

	print *, image_index(cafr4, i4)
	print *, image_index(cafr8, i4)
	print *, image_index(cafx4, i4)
	print *, image_index(cafx8, i4)

	print *, image_index(cafl1, i4)
	print *, image_index(cafl2, i4)
	print *, image_index(cafl4, i4)
	print *, image_index(cafl8, i4)

	print *, image_index(cafch, i4)
        print *, image_index(cafl8(29), i4)


!!!!Invalid
	print *, image_index(i1, i4)
	print *, image_index(i2, i4)
	print *, image_index(i4, i4)
	print *, image_index(i8scal, i4)

	print *, image_index(r4, i4)
	print *, image_index(r8, i4)
	print *, image_index(cx4, i4)
	print *, image_index(cx8, i4)

	print *, image_index(l1, i4)
	print *, image_index(l2, i4)
	print *, image_index(l4, i4)
	print *, image_index(l8scal, i4)
	print *, image_index(ch, i4)

	print *, image_index(cafi4[1], i4)

end

