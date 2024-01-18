! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Feb. 02, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Select Type
!*
!*  SECONDARY FUNCTIONS TESTED : Selector
!*
!*  REFERENCE                  : Feature 219934.OO_poly
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!* Implicit
!* ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M
    TYPE  :: DT0
      CHARACTER(513) :: C0="0"
      CONTAINS
      PROCEDURE, PASS   :: SetObj
    END TYPE

    !TYPE, ABSTRACT, EXTENDS(DT0) :: DT1
    TYPE,  EXTENDS(DT0) :: DT1
      CHARACTER(1025) :: C1="1"
    END TYPE

    TYPE, EXTENDS(DT1) :: DT
      CHARACTER(2049) :: C2="2"
    END TYPE

    TYPE (DT), SAVE, TARGET :: V

    CONTAINS

    SUBROUTINE SetObj(Arg)
    CLASS(DT0) :: Arg
      Arg%C0 = "SetDT0"
    END SUBROUTINE

  END MODULE

  PROGRAM Implicit3
  USE M
  IMPLICIT CLASS(DT)(V,U)

  CALL Sub(V)

  CONTAINS

  SUBROUTINE Sub(U)

  SELECT TYPE (V=>U)
  CLASS IS (DT)
    IF (TRIM(V%C0) .NE. "0") STOP 20
    IF (TRIM(V%C1) .NE. "1") STOP 21
    IF (TRIM(V%C2) .NE. "2") STOP 22

    V%DT0%C0 ="?"
    V%DT1%C1 ="?"
    V%C2 ="?"

  CLASS DEFAULT
    STOP 40
  END SELECT

    IF (TRIM(V%C0) .NE. "?") STOP 30
    IF (TRIM(V%C1) .NE. "?") STOP 31
    IF (TRIM(V%C2) .NE. "?") STOP 32

  END SUBROUTINE

  END



