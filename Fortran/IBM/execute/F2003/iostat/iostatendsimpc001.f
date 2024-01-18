!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: 
! %GROUP: diag1.f
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
!*  TEST CASE TITLE            : func1 
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
!*  DESCRIPTION                : Ensure that basic funcationailty works for eof with created sequence file
	implicit none
	integer :: ios = 0
	character(4) :: dword
	open( 1, file='cfile1.txt', status='new' ) 
	       write(1,*) "boat"
	       write(1,*) "moat"
	       write(1,*) "rake"
	       write(1,*) "take"
	       write(1,*) "plan"
	close(1)
	open( 1, file='cfile1.txt', action='read' )
	do while( .not. is_iostat_end(ios) )
         read( 1,*,iostat=ios ) dword
         if (is_iostat_end(ios)) then
         dword="eof "
         endif
         write(6,*) "ios = ", ios
         write(6,*) "four letter word = ", dword
  	enddo
	end
