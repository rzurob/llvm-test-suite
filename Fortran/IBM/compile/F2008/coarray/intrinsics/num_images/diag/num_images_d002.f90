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
!*  DESCRIPTION                : Return type testing. Num_images returns an int.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program main
	real*8, save :: caf[*]
	real*8, save :: caf_arr(10)[*]
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
	i1 = num_images()
	i2 = num_images()
	i4 = num_images()
	i8 = num_images()
	r4 = num_images()
	r8 = num_images()
	cx4 = num_images()
	cx8 = num_images()
	caf = num_images()


!!!! Invalid
	l1 = num_images()
	l2 = num_images()
	l4 = num_images()
	l8 = num_images()
	ch = num_images()
	lcaf = num_images()
end
