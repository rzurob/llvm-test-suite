! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: 
! %GROUP: iostatspeceor001.f
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
!*  TEST CASE TITLE            : iostateorspec
!*
!*  PROGRAMMER                 : Rob Wheeler
!*  DATE                       : Jan 20, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : is_iostat_eor 
!*  SECONDARY FUNCTIONS TESTED : None 
!*
!*  DRIVER STANZA              : xlf
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  DESCRIPTION                : Ensure that basic funcationailty works for instrinsic in spec expression
	program iostatspec
	  
	  integer :: iarr( size((/is_iostat_eor(-1),is_iostat_eor(-1),is_iostat_eor(-1)/)) )
	  integer :: iarr2( size((/is_iostat_eor(-1),is_iostat_eor(-1),is_iostat_eor(8),is_iostat_eor(8)/)) )
	  
	  
	  print *, "size of array 1:" ,size(iarr)
	  print *, "size of array 2:" ,size(iarr2)
	  
	  
	  
		
	end program iostatspec