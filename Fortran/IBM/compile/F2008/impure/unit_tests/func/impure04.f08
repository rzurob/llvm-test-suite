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
!*  DESCRIPTION                : Fortran 2008 support for the IMPURE attribute
!*                               for procedures,which allows for ELEMENTAL procedures
!*                               without the restrictions of PURE.
!*                               C1279 is not apply to IMPURE procedures
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

      IMPURE subroutine accumulate(dummy_procedure)
        implicit none
        integer dummy_procedure   ! pure subprogram is not required here
        integer res
        res = dummy_procedure()
        end
      END
