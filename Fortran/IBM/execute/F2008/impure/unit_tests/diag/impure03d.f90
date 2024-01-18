!*  ============================================================================
!*
!*  TEST CASE NAME             : impure03d.f
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
!*  DESCRIPTION                : IMPURE and PURE should not be both specified,C1244
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

      IMPURE ELEMENTAL PURE function accumulate(a)
        implicit none
        integer :: accumulate
        integer,intent(in) :: a
        accumulate = a + 1
      END
