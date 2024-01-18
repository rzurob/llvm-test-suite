! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP:  ArrFuncPtr.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : ArrFuncPtr
!*
!*  DATE                       : Nov. 02, 2004
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
!*    The selector is a function return  of array pointer
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

    TYPE :: Base
      INTEGER :: BaseId = 1
      CHARACTER(1023) :: C="!"
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetBaseId
      PROCEDURE, NoPASS :: SetId
      PROCEDURE, PASS   :: GetObj
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

    ELEMENTAL FUNCTION GetObj(Arg)
    CLASS(Base), INTENT(IN) :: Arg
    TYPE (Child)    :: GetObj
      SELECT TYPE (Arg)
      TYPE IS (Child)
        GetObj = Arg
      TYPE IS (Base)
        GetObj%Base = Arg
      CLASS DEFAULT
      END SELECT
    END FUNCTION

    SUBROUTINE SetId(Obj, Id)
    CLASS(Base)  :: Obj(:,:)
    INTEGER      :: Id
      SELECT TYPE (Obj)
      TYPE IS (Base)
        Obj%BaseId = Id
      TYPE IS (Child)
        Obj%ChildId = Id
      END SELECT
    END SUBROUTINE

    FUNCTION ReturnArr(Arg)
    TYPE (Child), Target :: Arg(:, :)
    TYPE (Child), POINTER :: ReturnArr(:, :)
      ReturnArr => Arg
    END FUNCTION
  END MODULE

  PROGRAM ArrFuncPtr
  USE M
  IMPLICIT NONE
  INTEGER :: i
  TYPE (Child), TARGET :: Arr(5, 5)
  INTEGER(8) :: S1(2)=(/5,4/), S2(2)=(/1,1/)

  Arr(4:5, 1)=Child(ChildID=-2, BaseID=-1)
  Arr(1, 1)  =Child(ChildID=-2, BaseID=-1)

  ASSOCIATE ( As => ReturnArr(Arr(S1, S2)), As1 => ReturnArr(Arr(S2, S2)) )

    IF ( ANY (LBOUND(As)  .NE. (/1,1/) ) )             STOP 30
    IF ( ANY (UBOUND(As)  .NE. (/2,2/) ) )             STOP 31
    IF ( ANY (SHAPE(As)   .NE. (/2,2/) ) )             STOP 32
    IF ( ANY (As%GetID()  .NE. RESHAPE((/-2,-2,-2,-2/), (/2,2/)) ) ) STOP 33
    IF ( ANY (As%ChildID  .NE. RESHAPE((/-2,-2,-2,-2/), (/2,2/)) ) ) STOP 34
    IF ( ANY (As%BaseID        .NE. RESHAPE((/-1,-1,-1,-1/), (/2,2/)) ) ) STOP 35
    IF ( ANY (As%Base%BaseID   .NE. RESHAPE((/-1,-1,-1,-1/), (/2,2/)) ) ) STOP 36
    IF ( LEN(As%C)        .NE. 1023)                   STOP 37
    IF ( TRIM(As(1,1)%C)  .NE. "!" )                   STOP 38

    ASSOCIATE( As => ReturnArr(As1) )

      IF ( ANY (LBOUND(As)  .NE. (/1,1/) ) )             STOP 40
      IF ( ANY (UBOUND(As)  .NE. (/2,2/) ) )             STOP 41
      IF ( ANY (SHAPE(As)   .NE. (/2,2/) ) )             STOP 42
      IF ( ANY (As%GetID()  .NE. RESHAPE((/-2,-2,-2,-2/), (/2,2/)) ) ) STOP 43
      IF ( ANY (As%ChildID  .NE. RESHAPE((/-2,-2,-2,-2/), (/2,2/)) ) ) STOP 44
      IF ( ANY (As%BaseID        .NE. RESHAPE((/-1,-1,-1,-1/), (/2,2/)) ) ) STOP 45
      IF ( ANY (As%Base%BaseID   .NE. RESHAPE((/-1,-1,-1,-1/), (/2,2/)) ) ) STOP 46
      IF ( LEN(As%C)        .NE. 1023)                   STOP 47
      IF ( TRIM(As(1,1)%C)  .NE. "!" )                   STOP 48

    END ASSOCIATE

  END ASSOCIATE

  END


