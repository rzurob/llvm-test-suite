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
!*  DESCRIPTION                : Invalid arguments test. Mix intrinsic calls
!*                               with variable statements.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program main
	implicit none
	real*8, save :: caf[*]
	real*8, save :: caf_arr(10)[*]

	integer :: num, x, y, z
	real*8 :: arr(10), c
	character(3) :: chr = ""

	num = num_images

	if (this_image() == 1) then
		num = num_images(1)
	end if

	num = num_images(this_image())
	call sub1(caf)

	x = 1
	y = 2
	z = 3
	num = num_images(x, y, z)
	num = num_images(chr)
end


subroutine sub1(coa)
	real*8, codimension[*] :: coa
	integer :: n
	complex :: x = 0
	logical :: y

	n = num_images(x, y)
end subroutine
