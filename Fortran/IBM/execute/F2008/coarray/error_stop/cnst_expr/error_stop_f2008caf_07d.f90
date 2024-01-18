!*  ============================================================================
!*
!*  TEST CASE NAME             : error_stop_f2008caf_07d.f
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
!*  Checking labeled error stop statement with constant character stop code
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program main
   implicit none
   character, parameter :: C = 'A'
   integer i

   do i = 1, 100
      if (i.GT.99) then
         goto 101
	  end if
   end do

101 ERROR STOP C

end program main
