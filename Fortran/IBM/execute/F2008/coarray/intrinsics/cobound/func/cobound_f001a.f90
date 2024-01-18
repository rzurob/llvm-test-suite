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
!*                               coarray types using 1 argument.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program main

	integer*4, save :: cafi4[1,*]
	integer*8, save :: cafi8(10)[9:10,0:*]
	real*4, save :: cafr4[1,*]
	real*8, save :: cafr8(1,2)[9:10,0:*]
	complex(4), save :: cafx4(3,3,3)[1,*]
	complex(8), save :: cafx8[9:10,0:*]
	character(4), save :: cafch4[1,*]
	character(8), save :: cafch8[9:10,0:*]
	logical*4, save :: cafl4[1,*]
	logical*8, save :: cafl8(55)[9:10,0:*]

	integer, dimension(2) :: arr1, arr2


!#### kind=4 type scalar and array coarrays
	arr1 = lcobound(cafi4)
	arr2 = ucobound(cafi4)
	print *, arr1, ":", arr2
	sync all

	arr1 = lcobound(cafr4)
	arr2 = ucobound(cafr4)
	print *, arr1, ":", arr2
	sync all

	arr1 = lcobound(cafx4)
	arr2 = ucobound(cafx4)
	print *, arr1, ":", arr2
	sync all

	arr1 = lcobound(cafch4)
	arr2 = ucobound(cafch4)
	print *, arr1, ":", arr2
	sync all

	arr1 = lcobound(cafl4)
	arr2 = ucobound(cafl4)
	print *, arr1, ":", arr2
	sync all


!#### kind=8 type scalar and array coarrays
	arr1 = lcobound(cafi8)
	arr2 = ucobound(cafi8)
	print *, arr1, ":", arr2
	sync all

	arr1 = lcobound(cafr8)
	arr2 = ucobound(cafr8)
	print *, arr1, ":", arr2
	sync all

	arr1 = lcobound(cafx8)
	arr2 = ucobound(cafx8)
	print *, arr1, ":", arr2
	sync all

	arr1 = lcobound(cafch8)
	arr2 = ucobound(cafch8)
	print *, arr1, ":", arr2
	sync all

	arr1 = lcobound(cafl8)
	arr2 = ucobound(cafl8)
	print *, arr1, ":", arr2
	sync all
end
