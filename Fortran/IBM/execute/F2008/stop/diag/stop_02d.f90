!*  ============================================================================
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
!*  Checking different variations of stop code values
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program main
   implicit none
   character, parameter :: C = '1'
   integer, parameter :: x = 10, y = 20, z = 30
   integer :: NONE = 0

   STOP -101
   STOP +101
   STOP x+y*z
   STOP LEN('x')+LEN('y')+LEN('z')
   STOP 10+KIND(C)+KIND(x)+KIND(y)+KIND(z)
   STOP 6+10*(2*3)

1001 STOP C         ! - No Error
1002 STOP NONE      ! - Error

end program main
