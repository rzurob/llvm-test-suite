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
!*  DESCRIPTION                : Test simple initialization with DATA for real coarray scalars.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program main
	real*4, save :: caf0[*], caf2[2,*]
	real*8, save :: caf1[-1:*], caf3[0:3,4,-1:*]

	interface
		subroutine sub1(c1, c2, c3, c4)
			real*4, save :: c1[*], c3[2,*]
			real*8, save :: c2[-1:*], c4[0:3,4,-1:*]
		end subroutine
	end interface

	data caf0,caf1,caf2,caf3/0., 1.0, 2.5, 3.25/

	call sub1(caf0, caf1, caf2, caf3)
end


subroutine sub1(c1, c2, c3, c4)
	real*4, save :: c1[*], c3[1,2,3,4,*]
	real*8, save :: c2[*], c4[0:3,4,-1:*]
	logical :: precision_r8, precision_r4

	if (.not. precision_r4(c1, 0.0_4)) then
		print *, c1
		error stop 21
	end if
	sync all

	if (.not. precision_r8(c2, 1.0_8)) then
		print *, c2
		error stop 22
	end if
	sync all

	if (.not. precision_r4(c3, 2.5_4)) then
		print *, c3
		error stop 23
	end if
	sync all

	if (.not. precision_r8(c4, 3.25_8)) then
		print *, c4
		error stop 24
	end if
end subroutine
