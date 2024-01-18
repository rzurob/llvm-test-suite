!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: iostatarrend004.f
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
!*  DESCRIPTION                : Ensure that basic funcationailty works for end with multidemnsion arrays
	implicit none
	integer :: ios3,ios4,ios5,iosall(3)
	integer, dimension(2,2) :: iosmult
	character(4) :: fourword
	character(5) :: fiveword
	character(3) :: threeword

        ios3 = 0
        ios4 = 0
        ios5 = 0
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

	iosmult(1,1)=ios5
	iosmult(1,2)=ios4

	iosmult(2,1)=ios5
	iosmult(2,2)=ios4

	write (6,*) "all my files are eof", all(is_iostat_end(iosmult))
	end
