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
!*  DESCRIPTION                : Ensure that basic funcationailty works for eof with stream file
	implicit none
	integer :: ios,j=1
	character(4) :: dword

	open( 1, file='stream1.txt', action='read', access='stream' )
        ios = 0
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
