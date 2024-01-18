! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: 
! %GROUP: iostatinteof001.f
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
!*  TEST CASE TITLE            : func8 
!*
!*  PROGRAMMER                 : Rob Wheeler
!*  DATE                       : Jan 16, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : is_iostat_end is_iostat_eor
!*  SECONDARY FUNCTIONS TESTED : None 
!*
!*  DRIVER STANZA              : xlf
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  DESCRIPTION                : Ensure that basic funcationailty works for eof internal file, seq, formatted
	CHARACTER(22):: X="these are the contents"
	CHARACTER(5):: C1
	character(4):: c2,c3
	character(9):: c4,c5,c6
	integer :: ios
	CHARACTER(23):: c7
	
	call setrteopts("iostat_end=2003std")
	READ( X, '(A5)' ,iostat=ios) C1 
	WRITE( *, * )  "ios=",ios, " ",C1, is_iostat_end(ios)

	READ( X, '(A4,A4)' ,iostat=ios) c2,c3
	WRITE( *, * )  "ios=",ios, " ",c2,c3, is_iostat_end(ios)
	
	READ( X, '(A9)' ,iostat=ios) c4
	WRITE( *, * )  "ios=",ios, " ",c4, is_iostat_end(ios)
	
	READ( X, '(A9)' ,iostat=ios) c5
	WRITE( *, * )  "ios=",ios, " ",c5, is_iostat_end(ios)
	
	READ( c3, '(A4,/,A4)' ,iostat=ios) c2,c3
	WRITE( *, * )  "ios=",ios, " ",c2,c3, is_iostat_end(ios)
	
	END
