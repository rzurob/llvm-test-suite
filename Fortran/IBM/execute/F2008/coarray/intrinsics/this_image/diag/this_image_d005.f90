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
!*  DESCRIPTION                : Return type testing. This_image returns a rank 1
!*				 integer array when 1 arg is properly provided.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program main
	real*8, save :: rcaf[*], rcaf2(10)[*]
	integer, save :: icaf[*]
	integer, allocatable :: aicaf(:)[*]
	logical, save :: lcaf[*]
	complex(4), save :: cxcaf(3,3)[*]

	integer*1, allocatable :: i1(:)
	integer*2, allocatable :: i2(:)
	integer*4, allocatable :: i4(:)
	integer*8, allocatable :: i8(:)
	real*8, allocatable :: r8(:)
	real*4, allocatable :: r4(:)
	complex(4), allocatable :: cx4(:)
	complex(8), allocatable :: cx8(:)
	character(3), allocatable :: ch(:)
	logical*1, allocatable :: l1(:)
	logical*2, allocatable :: l2(:)
	logical*4, allocatable :: l4(:)
	logical*8, allocatable :: l8(:)


!!!! Valid
	i1 = this_image(rcaf)
	i2 = this_image(cxcaf)
	i4 = this_image(icaf)
	i8 = this_image(lcaf)
	r4 = this_image(rcaf)
	r8 = this_image(icaf)
	cx4 = this_image(lcaf)
	cx8 = this_image(cxcaf)
	aicaf = this_image(icaf)


!!!! Invalid
	l1 = this_image(rcaf)
	l2 = this_image(lcaf)
	l4 = this_image(icaf)
	l8 = this_image(cxcaf)
	ch = this_image(icaf)
end

