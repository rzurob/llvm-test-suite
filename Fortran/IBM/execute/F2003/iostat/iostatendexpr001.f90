!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: iostatendexpr001.f
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
!*  DESCRIPTION                : Ensure that basic funcationailty works for eof and also expressions
	implicit none
	integer :: ios
	character(4) :: dword

        ios = 0
	open( 1, file='file1.txt', action='read' )
	do while( .not. is_iostat_end(ios) )
         read( 1,*,iostat=ios ) dword
         if (is_iostat_end(ios)) then
         dword="eof "
         endif
         write(6,*) "ios = ", ios
         write(6,*) "four letter word = ", dword
  enddo

     write(6,*) "expr = ios(which should be -1)+2+(2*4)-8-2: ",is_iostat_end(ios+2+(2*4)-8-2)
     write(6,*) "expr = ios(which should be -1)+2*max(3,4)-8: ",is_iostat_end(ios+2*max(3,4)-8)
     write(6,*) "expr = ios(which should be -1)+2*max(3,4)-8**3: ",is_iostat_end(ios+2*max(3,4)-8**3)
	end
