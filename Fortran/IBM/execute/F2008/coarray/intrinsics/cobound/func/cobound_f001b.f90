!234567890123456789012345678901234567890123456789012345678901234567890
!*  ===================================================================
!*
!*  DATE                       : September 2010
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Coarray
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Test lcobound/ucobound with different
!*                               coarray types using 2 arguments.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program main

	integer*1, save :: cafi1(2)[*]
	integer*8, save :: cafi8(2,2)[2,-3:-1,*]
	real*8, save :: cafr8(1,2,3)[2,-3:-1,*]
	complex(8), save :: cafx8[2,-3:-1,*]
	character(1), save :: cafch1[*]
	character(8), save :: cafch8[2,-3:-1,*]
	logical*1, save :: cafl1(5,5,5,5)[*]
	logical*8, save :: cafl8(55)[2,-3:-1,*]

	integer :: lo_1, lo_2, lo_3
	integer :: hi_1, hi_2, hi_3


!#### kind=1 type scalar and array coarrays
	lo_1 = lcobound(cafi1, 1)
	hi_1 = ucobound(cafi1, 1)
	print *, lo_1, ":", hi_1
	sync all

	lo_1 = lcobound(cafch1, 1)
	hi_1 = ucobound(cafch1, 1)
	print *, lo_1, ":", hi_1
	sync all

	lo_1 = lcobound(cafl1, 1)
	hi_1 = ucobound(cafl1, 1)
	print *, lo_1, ":", hi_1
	sync all


!#### kind=8 type scalar and array coarrays
	lo_1 = lcobound(cafi8, 1)
	lo_2 = lcobound(cafi8, 2)
	lo_3 = lcobound(cafi8, 3)
	hi_1 = ucobound(cafi8, 1)
	hi_2 = ucobound(cafi8, 2)
	hi_3 = ucobound(cafi8, 3)
	print *, lo_1, lo_2, lo_3, ":", hi_1, hi_2, hi_3
	sync all

	lo_1 = lcobound(cafr8, 1)
	lo_2 = lcobound(cafr8, 2)
	lo_3 = lcobound(cafr8, 3)
	hi_1 = ucobound(cafr8, 1)
	hi_2 = ucobound(cafr8, 2)
	hi_3 = ucobound(cafr8, 3)
	print *, lo_1, lo_2, lo_3, ":", hi_1, hi_2, hi_3
	sync all

	lo_1 = lcobound(cafx8, 1)
	lo_2 = lcobound(cafx8, 2)
	lo_3 = lcobound(cafx8, 3)
	hi_1 = ucobound(cafx8, 1)
	hi_2 = ucobound(cafx8, 2)
	hi_3 = ucobound(cafx8, 3)
	print *, lo_1, lo_2, lo_3, ":", hi_1, hi_2, hi_3
	sync all

	lo_1 = lcobound(cafch8, 1)
	lo_2 = lcobound(cafch8, 2)
	lo_3 = lcobound(cafch8, 3)
	hi_1 = ucobound(cafch8, 1)
	hi_2 = ucobound(cafch8, 2)
	hi_3 = ucobound(cafch8, 3)
	print *, lo_1, lo_2, lo_3, ":", hi_1, hi_2, hi_3
	sync all

	lo_1 = lcobound(cafl8, 1)
	lo_2 = lcobound(cafl8, 2)
	lo_3 = lcobound(cafl8, 3)
	hi_1 = ucobound(cafl8, 1)
	hi_2 = ucobound(cafl8, 2)
	hi_3 = ucobound(cafl8, 3)
	print *, lo_1, lo_2, lo_3, ":", hi_1, hi_2, hi_3
	sync all

end
