! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: SltFuncArgPtr.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : SltFuncArgPtr
!*
!*  DATE                       : Jan. 06, 2005
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
!*   The selector is a function return with associate name(to a pointer) as argument
!*    (297727)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

    TYPE  :: Zero
    END TYPE

    TYPE, EXTENDS(Zero)  :: Base
      INTEGER :: BaseId = 1
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetBaseId
      PROCEDURE, PASS   :: SetId => SetBaseId
    END TYPE

    TYPE, EXTENDS(Base) :: Child
      INTEGER  :: ChildId = 2
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetChildId
      PROCEDURE, PASS   :: SetId => SetChildId
    END TYPE

    CONTAINS

    ELEMENTAL FUNCTION GetChildId(Arg)
    CLASS(Child), INTENT(IN) :: Arg
    INTEGER                  :: GetChildId
      GetChildId = Arg%ChildId
    END FUNCTION

    ELEMENTAL FUNCTION GetBaseId(Arg)
    CLASS(Base), INTENT(IN)  :: Arg
    INTEGER                  :: GetBaseId
      GetBaseId = Arg%BaseId
    END FUNCTION

    SUBROUTINE SetBaseId(Arg)
    CLASS(Base)  :: Arg
      Arg%BaseId =  -Arg%BaseId
    END SUBROUTINE

    SUBROUTINE SetChildId(Arg)
    CLASS(Child)  :: Arg
      Arg%ChildId = -Arg%ChildId
    END SUBROUTINE

  END MODULE


  PROGRAM SltFuncArgPtr
  USE M
  IMPLICIT NONE
  TYPE(Child), TARGET :: Tar

  SELECT TYPE( As => Fun1(Tar))
  END SELECT

  CONTAINS

  FUNCTION Fun0(Arg)
  CLASS(Zero), TARGET ::Arg
  CLASS(*), POINTER :: Fun0
    Fun0 => Arg
  END FUNCTION

  RECURSIVE FUNCTION Fun1(Arg)
  CLASS(Zero), TARGET ::Arg
  CLASS(Zero), POINTER :: Fun1

  SELECT TYPE ( As0 => Fun0(Arg) )
    CLASS IS (Child)
      SELECT TYPE (As => Fun0(As0))
        TYPE IS (Child)
          IF ( As%Base%GetId() .NE. 1 ) STOP 34
          IF ( As%GetId()      .NE. 2 ) STOP 35
          IF ( As%BaseId       .NE. 1 ) STOP 36
          IF ( As%ChildId      .NE. 2 ) STOP 37

          CALL As0%SetId()
          CALL As0%Base%SetId()

          IF ( As%Base%GetId() .NE. -1 ) STOP 44
          IF ( As%GetId()      .NE. -2 ) STOP 45
          IF ( As%BaseId       .NE. -1 ) STOP 46
          IF ( As%ChildId      .NE. -2 ) STOP 47
       CLASS DEFAULT
          STOP 40
      END SELECT

    TYPE is (Base)
      STOP 32
    TYPE IS (Zero)
      STOP 38

  END SELECT

  Fun1 => NULL()

  END FUNCTION

  END

