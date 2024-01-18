!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan 9, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : is_iostat_eor
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Ensure that basic funcationailty works for eor with no advance and external file
	implicit none
	integer :: ios
	character(4) :: dword

        open( 1, file='file1.txt', action='read', IOSTAT=ios )
	do while( .not. is_iostat_eor(ios) )
         read( 1,'(A4)',iostat=ios ,advance='no') dword
         if (is_iostat_eor(ios)) then
         dword="eor "
         endif
         write(6,*) "ios = ", ios
         write(6,*) "four letter word = ", dword
	enddo
	end
