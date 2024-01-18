!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: iostatarrend005.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan 9, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : is_iostat_end
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Ensure that basic funcationailty works for end with zero sized arrays
	implicit none
	integer :: iosall(3)=(/1,2,3/),i1=5

	associate (z =>size(is_iostat_end(iosall(3:2))))

		write (6,*) "5 plus 0 : " , i1+z
		write (6,*) "5 times 0 : " , i1*z
	end associate

	end