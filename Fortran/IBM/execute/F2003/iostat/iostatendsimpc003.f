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
!*  DESCRIPTION                : Ensure that basic funcationailty works for eof with created stream file at last position
	implicit none
	integer :: ios
	character :: c1

	open( 1, file='cstream2.txt', status='new', access='stream' )
	       write(1) "thisisastreamfileforuseintesting"

         read( 1,POS=32,iostat=ios ) c1

         if (is_iostat_end(ios)) then
         c1='?'
         endif
         write(6,*) "ios = ", ios
         write(6,*) "character is = ", c1
         read( 1,POS=33,iostat=ios ) c1

         if (is_iostat_end(ios)) then
         c1='?'
         endif
         write(6,*) "ios = ", ios
         write(6,*) "character is = ", c1

	end