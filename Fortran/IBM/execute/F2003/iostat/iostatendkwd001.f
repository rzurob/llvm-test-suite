!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: iostatendkyd001.f
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
!*  PRIMARY FUNCTIONS TESTED   : is_iostat_end
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Ensure that basic funcationailty works for eof
	implicit none
	integer :: ios
	character(4) :: dword

        ios = 0
	open( 1, file='file1.txt', action='read' )
	do while( .not. is_iostat_end(ios) )
         read( 1,*,iostat=ios ) dword
         if (is_iostat_end(i=ios)) then
         dword="eof "
         print *,"small i ok"
         endif
         if (is_iostat_end(I=ios)) then
         dword="eof "
         print *,"big I ok"
         endif
         write(6,*) "ios = ", ios
         write(6,*) "four letter word = ", dword
  	enddo
	end
