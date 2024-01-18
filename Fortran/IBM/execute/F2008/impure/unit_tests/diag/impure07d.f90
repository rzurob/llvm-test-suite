!*  ============================================================================
!*
!*  TEST CASE NAME             : impure07d.f
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
!*  DESCRIPTION                : IMPURE ELEMENTAL procedures are not allowed with -qrecur
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

      IMPURE ELEMENTAL subroutine accumulate()
      END
