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
!*  DESCRIPTION                : Invalid parameter testing of lcobound
!*                               and ucobound. Test dim and kind without
!*                               a corray argument. Also test mismatching
!*                               argument tags.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program main

	integer, save :: caf[*]

	integer*1 :: i1
	integer*2 :: i2
	integer*4 :: i4
	integer*8 :: i8
	real*8 :: r8
	real*4 :: r4
	complex(4) :: cx4
	complex(8) :: cx8
	character(9) :: ch
	logical*1 :: l1
	logical*2 :: l2
	logical*4 :: l4
	logical*8 :: l8


!### lcobound
	print *, lcobound(COARRAY=i1)
	print *, lcobound(COARRAY=caf, 1)
	print *, lcobound(KIND=1, caf)
	print *, lcobound(COARRAY=caf, DIM=1, 4)
	print *, lcobound(COARRAY=caf, KIND=8, 1)
	print *, lcobound(DIM=1, KIND=2, caf)
	print *, lcobound(DIM=1, KIND=2)
	print *, lcobound(DIM=1)
	print *, lcobound(KIND=1)

	print *, lcobound(DIM=1, KIND=kind(i2), COARRAY=caf)	!valid
	print *, lcobound(DIM=1, COARRAY=caf, KIND=8)		!valid
	print *, lcobound(1, 1, caf)
	print *, lcobound(DIM=caf, COARRAY=1, KIND=kind(i8))
	print *, lcobound(COARRAY=caf, DIM=caf, KIND=caf)


!### ucobound
	print *, ucobound(COARRAY=i4)
	print *, ucobound(COARRAY=caf, 1)
	print *, ucobound(KIND=4, caf)
	print *, ucobound(COARRAY=caf, DIM=1, 8)
	print *, ucobound(COARRAY=caf, KIND=8, 2)
	print *, ucobound(DIM=1, KIND=1, caf)
	print *, ucobound(DIM=1, KIND=4)
	print *, ucobound(DIM=1)
	print *, ucobound(KIND=8)

	print *, ucobound(DIM=1, KIND=kind(i1), COARRAY=caf)	!valid
	print *, ucobound(DIM=1, COARRAY=caf, KIND=2)		!valid
	print *, ucobound(1, 1, caf)
	print *, ucobound(DIM=caf, COARRAY=1, KIND=kind(i4))

end
