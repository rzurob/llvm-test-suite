! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: 
! %GROUP: iostatspeceor002.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : iostatspeceor002
!*
!*  PROGRAMMER                 : Rob Wheeler
!*  DATE                       : Jan 20, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : is_iostat_eor
!*  SECONDARY FUNCTIONS TESTED : None 
!*
!*  DRIVER STANZA              : xlf
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
	 
	do while( .not. is_iostat_eor(ios) )
         read( 1,'(A4)',iostat=ios ,advance='no') dword
         if (is_iostat_eor(ios)) then
         dword="eor "
         call sub(ios)
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
	  real r1(func(is_iostat_eor(x)))
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
