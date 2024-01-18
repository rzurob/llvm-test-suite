!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: iostatarrend002.f
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
!*  PRIMARY FUNCTIONS TESTED   : is_iostat_eor
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Ensure that basic funcationailty works for eor with no advance and external file
	implicit none
	integer :: ios3=0,ios4=0,ios5=0,iosall(3),i
	character(4) :: fourword
	character(5) :: fiveword
	character(3) :: threeword
	call setrteopts("iostat_end=2003std")
	open( 1, file='file1.txt', action='read' )
	do while( .not. is_iostat_end(ios4) )
         read( 1,'(A4)',iostat=ios4 ) fourword
         if (is_iostat_end(ios4)) then
         fourword="end "
         endif
         write(6,*) "ios4 = ", ios4
         write(6,*) "four letter word = ", fourword
	enddo

	open( 1, file='file2.txt', action='read' )
	do while( .not. is_iostat_end(ios5) )
         read( 1,'(A5)',iostat=ios5 ) fiveword
         if (is_iostat_end(ios5)) then
         fiveword="end "
         endif
         write(6,*) "ios5 = ", ios5
         write(6,*) "five letter word = ", fiveword
	enddo

	open( 1, file='file3.txt', action='read' )
	do i=1,3
         read( 1,'(A3)',iostat=ios3 ) threeword
         if (is_iostat_end(ios3)) then
         threeword="end "
         endif
         write(6,*) "ios3 = ", ios3
         write(6,*) "three letter word = ", threeword
	enddo
	iosall=(/ios3,ios4,ios5/)
	write (6,*) "all my files are eof", all(is_iostat_end(iosall))
	end