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
!*  DESCRIPTION                : Return type testing of lcobound/ucobound
!*                               with 2 arguments returns a scalar integer.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program main

	integer, save :: caf[*]
	integer :: iarr(2)
	real :: rarr(2)
	complex :: carr(2)
	logical :: larr(2)

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
	i1 = lcobound(caf, 1)
	i2 = lcobound(caf, 1)
	i4 = lcobound(caf, 1)
	i8 = lcobound(caf, 1)

	r4 = lcobound(caf, 1)
	r8 = lcobound(caf, 1)
	cx4 = lcobound(caf, 1)
	cx8 = lcobound(caf, 1)

	l1 = lcobound(caf, 1)
	l2 = lcobound(caf, 1)
	l4 = lcobound(caf, 1)
	l8 = lcobound(caf, 1)
	ch = lcobound(caf, 1)

	caf = lcobound(caf, 1)
	caf[1] = lcobound(caf, 1)
	iarr = lcobound(caf, 1)
	iarr(1) = lcobound(caf, 1)
	carr = lcobound(caf, 1)


!### ucobound
	i1 = ucobound(caf, 1)
	i2 = ucobound(caf, 1)
	i4 = ucobound(caf, 1)
	i8 = ucobound(caf, 1)

	r4 = ucobound(caf, 1)
	r8 = ucobound(caf, 1)
	cx4 = ucobound(caf, 1)
	cx8 = ucobound(caf, 1)

	l1 = ucobound(caf, 1)
	l2 = ucobound(caf, 1)
	l4 = ucobound(caf, 1)
	l8 = ucobound(caf, 1)
	ch = ucobound(caf, 1)

	caf = ucobound(caf, 1)
	caf[1] = ucobound(caf, 1)
	iarr(2) = ucobound(caf, 1)
	rarr = ucobound(caf, 1)
	larr = ucobound(caf, 1)

end
