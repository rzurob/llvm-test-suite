!*  ============================================================================
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
!*  Checking variations of different constant and non constant expressions:
!*  LEN, KIND, loop counters and constants
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program main
   implicit none
   integer :: numImagesI = 3
   character :: numImagesC = '3'
   integer i
   integer, parameter :: N = 10

   ERROR STOP numImagesI          ! - Error
   ERROR STOP 101                 ! - No error
   ERROR STOP LEN("numImagesI")   ! - No error
   ERROR STOP KIND(numImagesI)    ! - No error
   ERROR STOP numImagesC          ! - Error
   ERROR STOP 'C'                 ! - No error
   ERROR STOP LEN(numImagesC)     ! - No error
   ERROR STOP KIND('C')           ! - No error

   do i = 1, N
      if (i.GT.5) then
         ERROR STOP N - 1         ! - No error
	  end if
      if (i.GT.N) then
         ERROR STOP i - 1         ! - Error
	  end if
   end do

end program main
