!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: 
! %GROUP: iostateorextadv001.f
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
!*  PRIMARY FUNCTIONS TESTED   : is_iostat_eor
!*  SECONDARY FUNCTIONS TESTED : None 
!*
!*  DRIVER STANZA              : xlf
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  DESCRIPTION                : Ensure that basic funcationailty works for eor with no advance and external file
	implicit none
	integer :: ios
	character(4) :: dword
	
        open( 1, file='file1.txt', action='read', IOSTAT=ios )
	do while( .not. is_iostat_eor(ios) )
         read( 1,'(A4)',iostat=ios ,advance='no') dword
         if (is_iostat_eor(ios)) then
         dword="eor "
         endif
         write(6,*) "ios = ", ios
         write(6,*) "four letter word = ", dword
	enddo
	end
