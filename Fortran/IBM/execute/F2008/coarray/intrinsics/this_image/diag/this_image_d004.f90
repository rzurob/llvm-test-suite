!234567890123456789012345678901234567890123456789012345678901234567890
!*  ===================================================================
!*
!*  DATE                       : July 2010
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Coarray
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Return type testing. This_image returns an scalar
!*				 integer when 0 or 2 args are properly provided.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program main
	real*8, save :: caf[*], caf_arr(10)[*]
	integer, save :: icaf[*]
	logical, save :: lcaf[*]

	integer*1 :: i1
	integer*2 :: i2
	integer*4 :: i4
	integer*8 :: i8
	real*8 :: r8
	real*4 :: r4
	complex(4) :: cx4
	complex(8) :: cx8
	character(3) :: ch
	logical*1 :: l1
	logical*2 :: l2
	logical*4 :: l4
	logical*8 :: l8


!!!! Valid
	i1 = this_image()
	i2 = this_image()
	i4 = this_image()
	i8 = this_image()
	r4 = this_image()
	r8 = this_image()
	cx4 = this_image()
	cx8 = this_image()
	caf = this_image()

	i1 = this_image(icaf, 1)
	i2 = this_image(icaf, 1)
	i4 = this_image(icaf, 1)
	i8 = this_image(icaf, 1)
	r4 = this_image(icaf, 1)
	r8 = this_image(icaf, 1)
	cx4 = this_image(icaf, 1)
	cx8 = this_image(icaf, 1)
	caf = this_image(icaf, 1)


!!!! Invalid
	l1 = this_image()
	l2 = this_image()
	l4 = this_image()
	l8 = this_image()
	ch = this_image()
	lcaf = this_image()

	l1 = this_image(icaf, 1)
	l2 = this_image(icaf, 1)
	l4 = this_image(icaf, 1)
	l8 = this_image(icaf, 1)
	ch = this_image(icaf, 1)
	lcaf = this_image(icaf, 1)

end

