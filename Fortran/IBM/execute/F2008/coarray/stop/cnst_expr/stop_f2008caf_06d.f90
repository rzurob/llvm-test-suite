!*  ============================================================================
!*
!*  TEST CASE NAME             : stop_f2008caf_06d.f
!*
!*  DATE                       : 2010-10-20
!*
!*  PRIMARY FUNCTIONS TESTED   : STOP statement
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
!*  Checking labeled stop statement with constant integer stop code
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program main
   implicit none
   integer, parameter :: N = 100
   integer i

   do i = 1, N
      if (i.GT.99) then
         goto 1001
	  end if
   end do

1001 STOP N - 1

end program main
