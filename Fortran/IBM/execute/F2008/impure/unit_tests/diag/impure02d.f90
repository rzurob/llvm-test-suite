!*  ============================================================================
!*
!*  TEST CASE NAME             : impure02d.f
!*
!*  DATE                       : 2012-03-08
!*  ORIGIN                     : Compiler Development, IBM China Development Shanghai Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : impure procedures
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature Number 917300
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                : IMPURE can only be specified at most once,C1243
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

      IMPURE IMPURE subroutine accumulate(a)
        implicit none
        integer,intent(in) :: a
      END
