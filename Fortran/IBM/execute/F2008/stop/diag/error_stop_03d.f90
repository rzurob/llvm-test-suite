!*  ============================================================================
!*
!*  TEST CASE NAME             : error_stop_03d.f
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
!*  Checking different variations of stop code values:
!*  KIND(integer)
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program main
   implicit none
   integer :: numImagesI = 3
   integer i

   do i = 1, 10
      if (i.GT.5) then
         ERROR STOP KIND(numImagesI)
	  end if
   end do

end program main
