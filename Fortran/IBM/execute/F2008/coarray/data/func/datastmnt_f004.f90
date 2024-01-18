!234567890123456789012345678901234567890123456789012345678901234567890
!*  ===================================================================
!*
!*  DATE                       : November 2010
!*
!*  PRIMARY FUNCTIONS TESTED   : Coarray
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Test simple initialization with DATA for logical coarray scalars.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program main

	logical*1, save :: caf0[*]
	logical*2, save :: caf1[-1:*]
	logical*4, save :: caf2[2,*]
	logical*8, save :: caf3[0:1,3,0:*]
	integer :: f

	data caf0/.true./, caf1/.false./
	data caf2/.true./, caf3/.true./

	f = fun1()
	print *, f

contains

	integer function fun1()
		fun1 = 0

		if (.not. caf0) then
			print *, caf0
			error stop 21
		end if

		if (caf1) then
			print *, caf1
			error stop 22
		end if

		if (.not. caf2) then
			print *, caf2
			error stop 23
		end if

		if (.not. caf3) then
			print *, caf3
			error stop 24
		end if

		fun1 = 1
	end function

end
