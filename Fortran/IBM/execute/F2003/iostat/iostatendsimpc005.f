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
!*  PRIMARY FUNCTIONS TESTED   : is_iostat_end is_iostat_eor
!*  SECONDARY FUNCTIONS TESTED : None 
!*
!*  DRIVER STANZA              : xlf
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  DESCRIPTION                : Ensure that basic funcationailty works for eof with created direct file access that is to say it cannot happen
	implicit none
	character(10) :: dword,r1="RECORD 1",r2="RECORD 2",r3="RECORD 3"
	
	integer :: ios
	OPEN (3,FILE='cdirect.txt',  status='new',ACCESS='DIRECT',RECL=10)
	write (3,rec=1) r1
	write (3,rec=2) r2
	write (3,rec=3) r3
	read (3,rec=1,iostat=ios) dword
	write (6,*) dword,ios,is_iostat_end(ios),is_iostat_eor(ios)
	read (3,rec=2,iostat=ios) dword
	write (6,*) dword,ios,is_iostat_end(ios),is_iostat_eor(ios)
	!this is past the end of the file...but is not eof or eor
	read (3,rec=4,iostat=ios) dword
	write (6,*) dword,ios,is_iostat_end(ios),is_iostat_eor(ios)
	CLOSE (3)
	end