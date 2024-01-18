!*  ============================================================================
!*
!*  TEST CASE NAME             : impure05f.f
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
!*                               for procedures,which allows for ELEMENTAL procedures
!*                               without the restrictions of PURE.
!*                               C1282 is not apply to IMPURE procedures
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

      IMPURE ELEMENTAL subroutine accumulate(para)
        implicit none
        integer, intent(inout) :: para
        integer,volatile :: a  ! volatile is allowed here
      END
