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
!*
!*  DATE                       : Jan 9, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : is_iostat_end
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Ensure that basic funcationailty works for eof with created stream file
	implicit none
	integer :: ios,j=1
	character(4) :: dword

        ios = 0
	open( 1, file='cstream3.txt', status='new', access='stream' )
	       write(1) "thisisastreamfileforuseintesting"
	do while( .not. is_iostat_end(ios) )

         read( 1,POS=j,iostat=ios ) dword
         j=j+4
         if (is_iostat_end(ios)) then
         dword="eof "
         endif
         write(6,*) "ios = ", ios
         write(6,*) "four letter word = ", dword
	enddo
	end
