!*  ============================================================================
!*
!*  DATE                       : 2012-03-08
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

      IMPURE ELEMENTAL IMPURE function accumulate(a)
        implicit none
        integer :: accumulate
        integer,intent(in) :: a
        accumulate = a + 1
      END
