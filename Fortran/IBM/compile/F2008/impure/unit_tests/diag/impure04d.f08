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
!*  DESCRIPTION                : IMPURE and PURE should not be both specified,C1244
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

      PURE ELEMENTAL IMPURE subroutine accumulate(a)
        implicit none
        integer,intent(in) :: a
      END