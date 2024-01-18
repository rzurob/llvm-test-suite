!*  ============================================================================
!*
!*  TEST CASE NAME             : impure08d.f
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
!*  DESCRIPTION                : Fortran 2008 support for the IMPURE attribute
!*                               for procedures, C1281
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789
      PURE ELEMENTAL SUBROUTINE accumulate(a)
        INTEGER, INTENT(IN) :: a

        contains
          IMPURE ELEMENTAL subroutine sub(b)
            real, intent(inout) ::  b
          end subroutine sub
      END SUBROUTINE
