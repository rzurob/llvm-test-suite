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
!* Procedure Interface
!* ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890




  MODULE M
    TYPE  :: DT0
      CHARACTER(513) :: C0="0"
      CONTAINS
      PROCEDURE, PASS   :: GetChar
    END TYPE

    !TYPE, ABSTRACT, EXTENDS(DT0) :: DT1
    TYPE,  EXTENDS(DT0) :: DT1
      CHARACTER(513) :: C1="1"
    END TYPE

    TYPE, EXTENDS(DT1) :: DT
      CHARACTER(513) :: C2="2"
    END TYPE

    TYPE (DT), SAVE, TARGET :: V

    CONTAINS

    FUNCTION GetChar(Arg)
    CLASS(DT0) :: Arg
    CHARACTER(513), ALLOCATABLE :: GetChar
      SELECT TYPE (Arg)
      TYPE IS (DT0)
        ALLOCATE(GetChar, SOURCE=Arg%C0)
      TYPE IS (DT1)
        ALLOCATE(GetChar, SOURCE=Arg%C1)
      TYPE IS (DT)
        ALLOCATE(GetChar, SOURCE=Arg%C2)
      CLASS DEFAULT
        STOP 20
      END SELECT
    END FUNCTION

  END MODULE

  PROGRAM ProcInterface1
  USE M
  IMPLICIT CLASS(DT)(U)

  CALL Sub(DT())

  CONTAINS

  SUBROUTINE Sub(UArg)

  INTERFACE
    FUNCTION UFun(UArg)
      CLASS(*), POINTER :: UFun
      CLASS(*) :: UArg
    END FUNCTION
  END INTERFACE


  SELECT TYPE ( V =>UFun(UArg) )
  CLASS IS (DT)

    IF (TRIM(V%C0) .NE. "0") STOP 30
    IF (TRIM(V%C1) .NE. "1") STOP 31
    IF (TRIM(V%C2) .NE. "2") STOP 32

    IF (TRIM(V%DT0%GetChar()) .NE. "0") STOP 40
    IF (TRIM(V%DT1%GetChar()) .NE. "1") STOP 41
    IF (TRIM(V%GetChar())     .NE. "2") STOP 42

  CLASS DEFAULT
    STOP 50
  END SELECT

  END SUBROUTINE

  END

  FUNCTION UFun(UArg)
  IMPLICIT NONE
  CLASS(*), POINTER :: UFun
  CLASS(*) :: UArg
    ALLOCATE(UFun, SOURCE=UArg)
  END FUNCTION


