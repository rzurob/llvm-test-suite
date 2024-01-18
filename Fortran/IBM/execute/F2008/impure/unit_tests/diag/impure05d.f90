!*  ============================================================================
!*
!*  TEST CASE NAME             : impure05d.f
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
!*  DESCRIPTION                : When IMPURE is specified but -qlanglvl is lower
!*                               than F2008, then issue a langlvl(L) message
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

      IMPURE subroutine accumulate()
      END
