!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: 
! %GROUP: iostatnoadveor001.f
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
!*  TEST CASE TITLE            : iostatnoadveor001.f 
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
!*  DESCRIPTION                : Ensure that basic funcationailty works for eof not advancing  vs eor

	implicit none
	integer :: ios,i
	character(4) :: dword
	
	open( 1, file='file1.txt', action='read' )
	
	!read first 3 of 4 record
	do i=1, 3
	read( 1,*,iostat=ios ) dword 
	write(6,*) "ios = ", ios
	write(6,*) "four letter word = ", dword
	enddo
	
	!read 4th record
	read( 1,'(A4)',iostat=ios ,advance='no') dword
	write(6,*) "ios = ", ios
	write(6,*) "four letter word = ", dword
  
	!read again with advance=no
  	read( 1,'(A4)',iostat=ios ,advance='no') dword
  	if (is_iostat_eor(ios)) then
  	dword="eor "
  	endif
  	if (is_iostat_end(ios)) then
    	dword="eof "
  	endif
  	write(6,*) "ios = ", ios
  	write(6,*) "four letter word = ", dword
	
	end
