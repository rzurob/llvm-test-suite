!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan 9, 2006
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

	open( 1, file='file1.txt', action='read' )
        ios = 0
	do while( .not. is_iostat_end(ios) )
         read( 1,*,iostat=ios ) dword
         if (is_iostat_end(ios)) then
         dword="eof "
         endif
         write(6,*) "ios = ", ios
         write(6,*) "four letter word = ", dword
  	enddo
	end
