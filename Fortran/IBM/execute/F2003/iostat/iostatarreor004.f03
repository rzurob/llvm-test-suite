!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan 9, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : is_iostat_eor
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Ensure that basic funcationailty works for eor with zero sized arrays
	implicit none
	integer :: iosall(4)=(/1,2,3,-4/),i1=5

	associate (z =>size(is_iostat_eor(iosall(3:2))))

		write (6,*) "5 plus 0 : " , i1+z
		write (6,*) "5 times 0 : " , i1*z
	end associate

	end