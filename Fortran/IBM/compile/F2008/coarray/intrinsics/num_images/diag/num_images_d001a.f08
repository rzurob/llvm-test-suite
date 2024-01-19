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
!*  DESCRIPTION                : Invalid arguments test. No arguments allowed.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program main
	implicit none
	real*8, save :: caf[*]
	real*8, save :: caf_arr(10)[*]

	integer :: num, x, y, z
	real*8 :: arr(10), c
	character(3) :: chr = ""


!#### 1 Args
	if (this_image() == 1) then
		num = num_images(1)
	end if

!#### 2 Args
	print *, num_images(x, y)

!#### 3 Args
	print *, num_images(x, y, z)
	print *, num_images(caf, x, y)

end
