!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jun. 12, 2007
!*
!*  PRIMARY FUNCTIONS TESTED   : DERIVED TYPE PARAMETERS
!*
!*  SECONDARY FUNCTIONS TESTED : Data Object Declaration
!*
!*  REFERENCE                  : Feature Number 289057
!*
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  -- C570 (R543) If a SAVE statement with an omitted saved entity list occurs
!*     in a scoping unit, no other explicit occurrence of the SAVE attribute or
!*     SAVE statement is permitted in the same scoping unit.
!*
!*  (337906)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM dtpAttrSpecStmtSaveC570
  END

  SUBROUTINE S()

  TYPE :: DT0(K0, L0)
    INTEGER, KIND :: K0=1
    INTEGER, LEN  :: L0=1
    INTEGER(K0)   :: I(L0)=K0
  END TYPE

  SAVE

  TYPE(DT0(8,11)), SAVE :: T1

  TYPE(DT0(8,11)) :: T2
  SAVE T2

  END SUBROUTINE

