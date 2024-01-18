! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP:  AttrOptionalArr.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : AttrOptionalArr
!*
!*  DATE                       : Feb 22, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Associate
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature 219934
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*   The selector has an array with the optional attribute
!*
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M

    TYPE  :: Zero
      private
    END TYPE

    TYPE, EXTENDS(Zero)  :: Base
      INTEGER :: BaseId = 1
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetBaseId
      PROCEDURE, NoPASS   :: SetId => SetBaseId
    END TYPE

    TYPE, EXTENDS(Base) :: Child
      INTEGER  :: ChildId = 2
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetChildId
      PROCEDURE, NoPASS   :: SetId => SetChildId
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
    CLASS(Base) :: Arg(:)
      Arg%BaseId =  -Arg%BaseId
    END SUBROUTINE

    SUBROUTINE SetChildId(Arg)
    CLASS(Base)  :: Arg(:)
      SELECT TYPE(Arg)
        TYPE IS (Child)
          Arg%ChildId = -Arg%ChildId
      END SELECT
    END SUBROUTINE


  END MODULE


  PROGRAM AttrOptionalArr
  USE M
  INTEGER :: i
  TYPE (Child) :: W(3)=Child(BaseID=-1, ChildID=-2)

  INTERFACE
    SUBROUTINE Sub(Arg, I, J)
    IMPORT Zero
    CLASS(Zero), OPTIONAL :: Arg(I:J)
    INTEGER :: I, J
    END SUBROUTINE
  END INTERFACE

  CALL Sub(W, 3, 5)

  IF ( ANY(W%BaseID        .NE. 1 )) STOP 40
  IF ( ANY(W%Base%GetId()  .NE. 1 )) STOP 41
  IF ( ANY(W%ChildID       .NE. 2 )) STOP 42
  IF ( ANY(W%GetId()       .NE. 2 )) STOP 43

  END

  SUBROUTINE Sub(Arg, I, J)
  USE M
  CLASS(Zero), OPTIONAL :: Arg(I:J)
  INTEGER :: I, J

  CALL Sub0(Arg, I, J)

  CONTAINS

  SUBROUTINE Sub0(Arg, I, J)
  CLASS(Zero), OPTIONAL :: Arg(I:)
  INTEGER :: I, J

  IF ( .NOT. PRESENT(Arg) ) STOP  05

  IF ( .NOT. PRESENT (Arg) ) STOP 11
  IF ( ANY(LBOUND(Arg) .NE. (/I/) ) )     STOP 11
  IF ( ANY(SHAPE(Arg)  .NE. (/J-I+1/) ) ) STOP 12

  ASSOCIATE ( Arg => Arg )
  SELECT TYPE ( Arg )
  CLASS IS (Child)

    IF ( ANY(Arg%BaseID        .NE. -1 )) STOP 30
    IF ( ANY(Arg%Base%GetId()  .NE. -1 )) STOP 31
    IF ( ANY(Arg%ChildID       .NE. -2 )) STOP 32
    IF ( ANY(Arg%GetId()       .NE. -2 )) STOP 33

    CALL Arg%Base%SetID(Arg)
    CALL Arg%SetID(Arg)

  CLASS DEFAULT
    STOP 99
  END SELECT

  END ASSOCIATE

  END SUBROUTINE

  END SUBROUTINE

