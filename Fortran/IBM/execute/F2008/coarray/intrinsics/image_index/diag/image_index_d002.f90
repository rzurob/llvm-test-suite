!234567890123456789012345678901234567890123456789012345678901234567890
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : image_index_d002.f
!*
!*  PROGRAMMER                 : Francesco Cassullo
!*  DATE                       : August 2010
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Coarray
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Invalid arguments test. Second arg must
!*                               default integer rank-1 array.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program main
	integer*1, save :: i1, cafi1[*], iarr1
	integer*2, save :: i2, cafi2[*], iarr2
	integer*4, save :: i4, cafi4[*]
	integer*8, save :: i8, cafi8(10)[*], iarr8
	real*8, save :: r8, cafr8(1,2)[*], rarr8
	real*4, save :: r4, cafr4[*], rarr4
	complex(4), save :: cx4, cafx4(3,3,3)[*], cxarr4
	complex(8), save :: cx8, cafx8[*]
	character(3), save :: ch, cafch[*]
	logical*1, save :: l1, cafl1[*]
	logical*2, save :: l2, cafl2[*]
	logical*4, save :: l4, cafl4[*], larr4
	logical*8, save :: l8, cafl8(55)[*]
	integer, allocatable :: ialloc(:)


	print *, image_index(cafi1, i1)
	print *, image_index(cafi1, i2)
	print *, image_index(cafi2, i4)
	print *, image_index(cafi4, i8)

	print *, image_index(cafi8, r4)
	print *, image_index(cafr4, r8)
	print *, image_index(cafr8, cx4)
	print *, image_index(cafx4, cx8)
	print *, image_index(cafx8, ch)

	print *, image_index(cafl1, l1)
	print *, image_index(cafl2, l2)
	print *, image_index(cafl4, l4)
	print *, image_index(cafl8, l8)

	print *, image_index(cafi1, iarr1)
	print *, image_index(cafi2, iarr2)
	print *, image_index(cafi4, iarr8)
	print *, image_index(cafi8, rarr8)
	print *, image_index(cafr4, rarr4)
	print *, image_index(cafr8, cxarr4)
	print *, image_index(cafl1, larr4)
	
	print *, image_index(cafl2, cafl2)
	print *, image_index(cafx4, cafl1[1])

end

