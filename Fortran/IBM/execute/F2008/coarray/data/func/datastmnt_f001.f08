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
!*  DESCRIPTION                : Test simple initialization with DATA for integer coarray scalars.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module modFDC
	integer*1, save :: caf0[*]
	integer*2, save :: caf1[-1:*]
	integer*4, save :: caf2[2,*]
	integer*8, save :: caf3[0:1,3,0:*]

	data caf0,caf1/99/
	data caf2/66/
	data caf3/77/
end module


program main
	use modFDC

	if (caf0 /= 99) then
		print *, caf0
		error stop 21
	end if
	sync all

	if (caf1 /= 0) then
		print *, caf1
		error stop 22
	end if
	sync all

	if (caf2 /= 66) then
		print *, caf2
		error stop 23
	end if
	sync all

	if (caf3 /= 77) then
		print *, caf3
		error stop 24
	end if
end
