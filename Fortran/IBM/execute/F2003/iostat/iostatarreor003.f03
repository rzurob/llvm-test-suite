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
	integer :: ios3,ios4,ios5,iosall(3),i
	character(4) :: fourword
	character(5) :: fiveword
	character(3) :: threeword
	integer, dimension(2,2) :: iosmult

        ios3=0; ios4=0; ios5=0; iosall=0
	open( 1, file='file1.txt', action='read' )
	do while( .not. is_iostat_eor(ios4) )
         read( 1,'(A4)',iostat=ios4,advance="no" ) fourword
         if (is_iostat_eor(ios4)) then
         fourword="eor "
         endif
         write(6,*) "ios4 = ", ios4
         write(6,*) "four letter word = ", fourword
	enddo

	open( 1, file='file2.txt', action='read' )
	do while( .not. is_iostat_eor(ios5) )
         read( 1,'(A5)',iostat=ios5 ,advance="no") fiveword
         if (is_iostat_eor(ios5)) then
         fiveword="eor "
         endif
         write(6,*) "ios5 = ", ios5
         write(6,*) "five letter word = ", fiveword
	enddo

	open( 1, file='file3.txt', action='read' )
	do i=1,1
         read( 1,'(A3)',iostat=ios3 ,advance="no") threeword
         if (is_iostat_eor(ios3)) then
         threeword="eor "
         endif
         write(6,*) "ios3 = ", ios3
         write(6,*) "three letter word = ", threeword
	enddo
	iosall=(/ios3,ios4,ios5/)

	iosmult(1,1)=ios5
	iosmult(1,2)=ios4

	iosmult(2,1)=ios5
	iosmult(2,2)=ios4
	write (6,*) "all my files are eor", all(is_iostat_eor(iosmult))
	end
