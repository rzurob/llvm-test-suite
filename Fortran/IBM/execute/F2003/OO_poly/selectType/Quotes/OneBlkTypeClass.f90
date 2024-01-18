! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: OneBlkTypeClass.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : OneBlkTypeClass
!*
!*  DATE                       : Dec. 13, 2004
!*
!*  PRIMARY FUNCTIONS TESTED   : Select Type
!*
!*  SECONDARY FUNCTIONS TESTED : ONLY ONE TYPE/CLASS IS BLOCK
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
!*   The type /class is bolck is specified with the same type spec
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890




  MODULE M

    TYPE, ABSTRACT ::  Zero
    END TYPE

    TYPE, EXTENDS(Zero) :: Base
      INTEGER :: BaseId = 1
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetBaseId
    END TYPE

    TYPE, EXTENDS(Base) :: Child
      INTEGER  :: ChildId = 2
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetChildId
    END TYPE

    CONTAINS

    ELEMENTAL FUNCTION GetChildId(Arg)
    CLASS(Child), INTENT(IN) :: Arg
    INTEGER      :: GetChildId
      GetChildId = Arg%ChildId
    END FUNCTION

    ELEMENTAL FUNCTION GetBaseId(Arg)
    CLASS(Base), INTENT(IN) :: Arg
    INTEGER      :: GetBaseId
      GetBaseId = Arg%BaseId
    END FUNCTION

  END MODULE



  PROGRAM OneBlkTypeClass
  USE M
  IMPLICIT NONE

  INTEGER :: Visited = 0

  IF (Fun(Base())) THEN
    IF (Visited .NE. 1 ) THEN
       STOP 111
    END IF
  END IF

  Visited = 0
  IF (Fun(Child())) THEN
    IF (Visited .NE. 1 ) THEN
       STOP 112
    END IF
  END IF

  Visited = 0
  IF (Fun(1_1)) THEN
    IF (Visited .NE. 1 ) THEN
       STOP 113
    END IF
  END IF

  CONTAINS

  LOGICAL FUNCTION Fun(Arg)
  CLASS(*) :: Arg

  SELECT TYPE ( Arg )
    CLASS DEFAULT
      Visited = Visited + 1
    TYPE IS (Child)
      Visited = Visited + 1
    CLASS IS (Base)
      Visited = Visited + 1
    CLASS IS (Child)
      Visited = Visited + 1
    TYPE IS (Base)
      Visited = Visited + 1
  END SELECT

  Fun = .TRUE.

  END FUNCTION

  END

