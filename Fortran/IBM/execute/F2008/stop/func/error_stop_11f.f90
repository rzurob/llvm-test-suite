!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : error_stop_11f.f
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Shahid Alam
!*  DATE                       : 2010-10-20
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : ERROR STOP statement
!*
!*  SECONDARY FUNCTIONS TESTED : See the description below
!*
!*  REFERENCE                  : Feature Number 381018
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : 
!*  TARGET(S)                  : 
!*  NUMBER OF TESTS CONDITIONS : 
!*
!*  DESCRIPTION
!*
!*  This program tests the exit status of the ERROR STOP statement (constant integer):
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module stop_data
   character, PARAMETER :: fibonacci_N = '9'
   character, PARAMETER :: fibonacci_NMinus = '8'
end module

module fibonnaci_module

   use stop_data
   implicit none
   contains

   subroutine fibonacci(N)

      integer, intent(in) :: N
	  integer, dimension(N + 1) :: A
      integer i

      A(1) = 0
	  A(2) = 1
      do i = 3, N + 1
	     if (i .GT. N + 1) then
		    ERROR STOP fibonacci_NMinus
		 end if
	     A(i) = A(i - 2) + A(i - 1)
      end do

	  print *, A(N + 1)
      ERROR STOP fibonacci_N

   end subroutine fibonacci

end module

program main

   use fibonnaci_module
   implicit none

   call fibonacci(9)

end program main
