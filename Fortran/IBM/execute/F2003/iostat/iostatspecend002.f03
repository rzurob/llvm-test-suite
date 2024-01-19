!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan 20, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : is_iostat_end
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Ensure that basic funcationailty works for instrinsic in spec expression
	program iostatspec

	interface

	pure integer function func(L)
		logical , intent(in) ::L
	end function func

	end interface
	integer :: ios
	character(4) :: dword
	open( 1, file='file1.txt', action='read' )
        ios = 0
	do while( .not. is_iostat_end(ios) )
         read( 1,*,iostat=ios ) dword
         if (is_iostat_end(ios)) then
         call sub(ios)
         dword="eof "
         else
         call sub(ios)
         endif

         write(6,*) "ios = ", ios
         write(6,*) "four letter word = ", dword
  	enddo

	contains

	subroutine sub(x)
	  integer :: x
		logical i
		real r1(func(is_iostat_end(x)))
		print *, "size of real array is: ",size(r1)
		end subroutine sub

	end program iostatspec

	pure integer function func(L)
		logical , intent(in) ::L
		if (l .eqv. .true.) then
			func=10
		else
			func=5
		endif
	end function func
