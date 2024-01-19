!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jun. 26, 2007
!*
!*  PRIMARY FUNCTIONS TESTED   : DERIVED CLASS PARAMETERS
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
!*  -- The implicit statement
!*  Diagnosis on the use of implicit typing on types with deferred/assumed type parameters
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM dtpImplicit7

  TYPE :: DT(L)
    INTEGER, LEN  :: L=1
  END TYPE

  IMPLICIT TYPE(DT(:))(R)
  IMPLICIT TYPE(DT(*))(S)

  IMPLICIT CLASS(DT(:))(U)
  IMPLICIT CLASS(DT(*))(V)

  TARGET R
  SAVE   S  ! However there is no constraint for this from std

  VOLATILE U
  TARGET   V


  END


