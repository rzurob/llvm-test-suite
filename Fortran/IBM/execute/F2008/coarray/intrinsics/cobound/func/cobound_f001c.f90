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
!*  DESCRIPTION                : Test lcobound/ucobound with different
!*                               coarray types using 3 arguments. Also
!*                               test the case with 2 arguments where one
!*                               is a corray and the other is the kind.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program main

	integer*2, save :: cafi2(1)[1,2,2,3:4,*]
	integer*4, save :: cafi4(1,2)[4,0:*]
	real*4, save :: cafr4[4,0:*]
	complex(4), save :: cafx4(10,10)[4,0:*]
	character(2), save :: cafch2[1,2,2,3:4,*]
	character(4), save :: cafch4[4,0:*]
	logical*2, save :: cafl2(1,2,3,4,5)[1,2,2,3:4,*]
	logical*4, save :: cafl4(100)[4,0:*]

	integer(1) :: arr1l(2), arr1u(2), lo_1(5), hi_1(5)
	integer(2) :: arr2l(2), arr2u(2), lo_2(5), hi_2(5)
	integer(4) :: arr4l(2), arr4u(2)
	integer(8) :: arr8l(2), arr8u(2), lo_8(5), hi_8(5)


!#### kind=2 type scalar and array coarrays
	lo_1(1) = lcobound(cafi2, 1, 1)
	lo_1(2) = lcobound(cafi2, 2, 1)
	lo_1(3) = lcobound(cafi2, 3, 1)
	lo_1(4) = lcobound(cafi2, 4, 1)
	lo_1(5) = lcobound(cafi2, 5, 1)
	hi_1(1) = ucobound(cafi2, 1, 1)
	hi_1(2) = ucobound(cafi2, 2, 1)
	hi_1(3) = ucobound(cafi2, 3, 1)
	hi_1(4) = ucobound(cafi2, 4, 1)
	hi_1(5) = ucobound(cafi2, 5, 1)
	print *, lo_1, ":", hi_1
	sync all

	lo_2(1) = lcobound(cafch2, 1, 2)
	lo_2(2) = lcobound(cafch2, 2, 2)
	lo_2(3) = lcobound(cafch2, 3, 2)
	lo_2(4) = lcobound(cafch2, 4, 2)
	lo_2(5) = lcobound(cafch2, 5, 2)
	hi_2(1) = ucobound(cafch2, 1, 2)
	hi_2(2) = ucobound(cafch2, 2, 2)
	hi_2(3) = ucobound(cafch2, 3, 2)
	hi_2(4) = ucobound(cafch2, 4, 2)
	hi_2(5) = ucobound(cafch2, 5, 2)
	print *, lo_2, ":", hi_2
	sync all

	lo_8(1) = lcobound(cafl2, 1, 8)
	lo_8(2) = lcobound(cafl2, 2, 8)
	lo_8(3) = lcobound(cafl2, 3, 8)
	lo_8(4) = lcobound(cafl2, 4, 8)
	lo_8(5) = lcobound(cafl2, 5, 8)
	hi_8(1) = ucobound(cafl2, 1, 8)
	hi_8(2) = ucobound(cafl2, 2, 8)
	hi_8(3) = ucobound(cafl2, 3, 8)
	hi_8(4) = ucobound(cafl2, 4, 8)
	hi_8(5) = ucobound(cafl2, 5, 8)
	print *, lo_8, ":", hi_8
	sync all


!#### kind=4 type scalar and array coarrays
	arr1l = lcobound(COARRAY=cafi4, KIND=1)
	arr1u = ucobound(COARRAY=cafi4, KIND=1)
	print *, arr1l, ":", arr1u
	sync all

	arr2l = lcobound(COARRAY=cafr4, KIND=2)
	arr2u = ucobound(COARRAY=cafr4, KIND=2)
	print *, arr2l, ":", arr2u
	sync all

	arr4l = lcobound(COARRAY=cafx4, KIND=4)
	arr4u = ucobound(COARRAY=cafx4, KIND=4)
	print *, arr4l, ":", arr4u
	sync all

	arr8l = lcobound(COARRAY=cafch4, KIND=8)
	arr8u = ucobound(COARRAY=cafch4, KIND=8)
	print *, arr8l, ":", arr8u
	sync all

	arr1l = lcobound(COARRAY=cafl4, KIND=1)
	arr8u = ucobound(COARRAY=cafl4, KIND=8)
	print *, arr1l, ":", arr8u
	sync all

end
