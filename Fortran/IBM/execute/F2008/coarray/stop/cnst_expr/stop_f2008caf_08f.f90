!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : stop_f2008caf_08f.f
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Shahid Alam
!*  DATE                       : 2010-10-20
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : STOP statement
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
!*  This program tests the exit status of the STOP statement (constant integer):
!*
!*  Initializes a matrix and stops at a specific value passed
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

subroutine loop_stop(stop_value)

   implicit none
   integer, intent(in) :: stop_value
   integer, parameter :: stop_code = 101
   integer, dimension (30, 30) :: A
   integer i, j

   A = 0
   do i = 1, 30
      do j = 1, 30
         if (i .NE. j) then
            A(i, j) = i + j
         else if (i .GT. stop_value) then
	    print *, "SUM(A) =",sum(A)
	    STOP stop_code
	    print *, "ERROR: After STOP"
         else
            A(i, j) = 1
         endif
      enddo
   enddo

end subroutine

program main

   call loop_stop(15)

end program main
