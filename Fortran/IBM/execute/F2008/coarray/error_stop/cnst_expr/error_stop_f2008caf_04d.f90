!*  ============================================================================
!*
!*  TEST CASE NAME             : error_stop_f2008caf_04d.f
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
!*  KIND(character)
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program main
   implicit none
   character :: numImagesC = '3'
   integer i

   do i = 1, 10
      if (i.GT.5) then
         ERROR STOP KIND(numImagesC)
	  end if
   end do

end program main
