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
!* Finalization
!* (ICE)
!* The selector is an array constructor- no finalization yet?
!*
!234567890123456789012345678901234567890123456789012345678901234567890




  MODULE M
    TYPE  :: DT0
      CHARACTER(513) :: C0="0"
      CONTAINS
      PROCEDURE, PASS  :: GetChar
      Final :: FinalDT0
    END TYPE

    TYPE,  EXTENDS(DT0) :: DT1
      CHARACTER(513) :: C1="1"
      CONTAINS
      Final :: FinalDT1
    END TYPE

    TYPE, EXTENDS(DT1) :: DT
      CHARACTER(513) :: C2="2"
      CONTAINS
      Final :: FinalDT
    END TYPE

    LOGICAL :: Final(0:2) = .FALSE.

    CONTAINS

    SUBROUTINE FinalDT0(Arg)
    TYPE(DT0) :: Arg
      Final(0) = .TRUE.
    END SUBROUTINE

    SUBROUTINE FinalDT1(Arg)
    TYPE(DT1) :: Arg
      Final(1) = .TRUE.
    END SUBROUTINE

    SUBROUTINE FinalDT(Arg)
    TYPE(DT) :: Arg
      Final(2) = .TRUE.
    END SUBROUTINE


    FUNCTION GetChar(Arg)
    CLASS(DT0) :: Arg
    CHARACTER(513) :: GetChar
      SELECT TYPE (Arg)
      TYPE IS (DT0)
        GetChar = Arg%C0
      TYPE IS (DT1)
        GetChar = Arg%C1
      TYPE IS (DT)
        GetChar = Arg%C2
      CLASS DEFAULT
        STOP 20
      END SELECT
    END FUNCTION

  END MODULE

  PROGRAM final2
  USE M
  IMPLICIT CLASS(*)(U)

  CALL Sub(DT(C2="-2", C1="-1", C0="-0") )
  CONTAINS

  SUBROUTINE  Sub(UArg)

  SELECT TYPE ( V => (/UArg/) )
  CLASS IS (DT)
  SELECT TYPE (V => V(1))
  CLASS DEFAULT
  SELECT TYPE (V)
  TYPE IS (DT)

    IF (TRIM(V%C0) .NE. "-0") ERROR STOP 30
    IF (TRIM(V%C1) .NE. "-1") ERROR STOP 31
    IF (TRIM(V%C2) .NE. "-2") ERROR STOP 32

    IF (TRIM(V%DT0%GetChar()) .NE. "-0") ERROR STOP 40
    IF (TRIM(V%DT1%GetChar()) .NE. "-1") ERROR STOP 41
    IF (TRIM(V%GetChar())     .NE. "-2") ERROR STOP 42

  CLASS DEFAULT
    STOP 50
  END SELECT
    IF (ANY(Final)) ERROR STOP 60
  END SELECT
    IF (ANY(Final)) ERROR STOP 61
  END SELECT

 !IF (ANY(Final .NEQV. .TRUE. )) ERROR STOP 62
  IF (ANY(Final))                ERROR STOP 62  ! No finalization on array constructor!

  END SUBROUTINE

  END
