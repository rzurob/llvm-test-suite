!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: 
! %GROUP: iostatendovf001.f 
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
!*  TEST CASE TITLE            : iostatendovf001.f 
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
!*  DESCRIPTION                : Insure integer overflow is handled
	implicit none
	integer :: ios
	
	
	print *, is_iostat_end(i=70000) !bigger than integer*2
	print *, is_iostat_end(i=4594967295) !bigger than integer*4
	print *, is_iostat_end(i=19446744073709551615) !bigger than integer*8
	print *, is_iostat_eor(i=70000) !bigger than integer*2
	print *, is_iostat_eor(i=4594967295) !bigger than integer*4
	print *, is_iostat_eor(i=19446744073709551615) !bigger than integer*8
	
	
	
	end
