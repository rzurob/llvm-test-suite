!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan 9, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : is_iostat_end is_iostat_eor
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Ensure that basic funcationailty works for eof with direct file access that is to say it cannot happen
	implicit none
	character(10) :: dword
	integer :: ios
	OPEN (3,FILE='direct.txt',  ACCESS='DIRECT',RECL=10)
	read (3,rec=1,iostat=ios) dword
	write (6,*) dword,ios,is_iostat_end(ios),is_iostat_eor(ios)
	read (3,rec=2,iostat=ios) dword
	write (6,*) dword,ios,is_iostat_end(ios),is_iostat_eor(ios)
	!this is past the end of the file...but is not eof or eor
	read (3,rec=4,iostat=ios) dword
	write (6,*) dword,ios,is_iostat_end(ios),is_iostat_eor(ios)
	CLOSE (3)
	end