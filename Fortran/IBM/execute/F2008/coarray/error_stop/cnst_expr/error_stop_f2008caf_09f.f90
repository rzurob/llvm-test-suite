!*  ============================================================================
!*
!*  TEST CASE NAME             : error_stop_f2008caf_09f.f
!*
!*  DATE                       : 2010-10-20
!*
!*  PRIMARY FUNCTIONS TESTED   : ERROR STOP statement
!*
!*  SECONDARY FUNCTIONS TESTED : See the description below
!*
!*  REFERENCE                  : Feature Number 381018
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  This program tests the exit status of the ERROR STOP statement (constant character):
!*
!*  Initializes a matrix and stops at a specific value passed
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789


subroutine loop_stop(stop_value)

   implicit none
   integer, intent(in) :: stop_value
   character, parameter :: stop_code = 'S'
   integer, dimension (30, 30) :: A
   integer i, j

   A = 0
   do i = 1, 30
      do j = 1, 30
         if (i .NE. j) then
            A(i, j) = i + j
         else if (i .GT. stop_value) then
	       print *, "SUM(A) = ", sum(A)
               ERROR STOP stop_code
         else
            A(i, j) = 1
         endif
      enddo
   enddo

end subroutine

program main

   call loop_stop(25)

end program main
