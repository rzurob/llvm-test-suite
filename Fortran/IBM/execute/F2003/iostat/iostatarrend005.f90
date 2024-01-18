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
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : iostatarrend005.f
!*
!*  PROGRAMMER                 : Rob Wheeler
!*  DATE                       : Jan 9, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : is_iostat_end
!*  SECONDARY FUNCTIONS TESTED : None 
!*
!*  DRIVER STANZA              : xlf
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