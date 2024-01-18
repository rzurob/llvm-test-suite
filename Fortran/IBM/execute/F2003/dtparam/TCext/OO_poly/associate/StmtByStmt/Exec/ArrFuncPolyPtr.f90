! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/associate/StmtByStmt/Exec/ArrFuncPolyPtr.f
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP:  ArrFuncPolyPtr.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : ArrFuncPolyPtr
!*
!*  DATE                       : Feb 14, 2005
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
!*    The selector is a function call returning an array pointer of poly
!*    (Comp failed)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

    TYPE :: Base(K1,K2,N1)    ! (4,1,1023)
      INTEGER, KIND             :: K1,K2
      INTEGER, LEN              :: N1
      INTEGER(K1)               :: BaseId = 1
      CHARACTER(kind=K2,len=N1) :: C="!"
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetBaseId
      PROCEDURE, NoPASS :: SetId
      PROCEDURE, PASS   :: GetObj
    END TYPE

    TYPE, EXTENDS(Base) :: Child    ! (4,1,1023)
      INTEGER(K1)  :: ChildId = 2
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetChildId
    END TYPE

    CONTAINS

    ELEMENTAL FUNCTION GetChildId(Arg)
    CLASS(Child(4,1,*)), INTENT(IN) :: Arg
    INTEGER      :: GetChildId
      GetChildId = Arg%ChildId
    END FUNCTION

    ELEMENTAL FUNCTION GetBaseId(Arg)
    CLASS(Base(4,1,*)), INTENT(IN) :: Arg
    INTEGER      :: GetBaseId
      GetBaseId = Arg%BaseId
    END FUNCTION

    ELEMENTAL FUNCTION GetObj(Arg)
    CLASS(Base(4,1,*)), INTENT(IN) :: Arg
    TYPE (Child(4,1,1023))    :: GetObj
      SELECT TYPE (Arg)
      TYPE IS (Child(4,1,*))
        GetObj = Arg
      TYPE IS (Base(4,1,*))
        GetObj%Base = Arg
      CLASS DEFAULT
      END SELECT
    END FUNCTION

    SUBROUTINE SetId(Obj, Id)
    CLASS(Base(4,1,*))  :: Obj(:,:)
    INTEGER      :: Id
      SELECT TYPE (Obj)
      TYPE IS (Base(4,1,*))
        Obj%BaseId = Id
      TYPE IS (Child(4,1,*))
        Obj%ChildId = Id
      END SELECT
    END SUBROUTINE

    FUNCTION ReturnArr(Arg)
    CLASS (Child(4,1,*)), Target :: Arg(:, :)
    CLASS (Child(4,1,:)), POINTER :: ReturnArr(:, :)
      ReturnArr => Arg
    END FUNCTION
  END MODULE

  PROGRAM ArrFuncPolyPtr
  USE M
  IMPLICIT NONE
  INTEGER :: i
  TYPE (Child(4,1,1023)), TARGET :: Arr(2, 2)=Child(4,1,1023)(ChildID=-2, BaseID=-1)
  INTEGER(8) :: S1(2)=(/5,4/), S2(2)=(/1,1/)

  ASSOCIATE ( As => ReturnArr(Arr) )

    IF ( ANY (LBOUND(As)  .NE. (/1,1/) ) )             STOP 30
    IF ( ANY (UBOUND(As)  .NE. (/2,2/) ) )             STOP 31
    IF ( ANY (SHAPE(As)   .NE. (/2,2/) ) )             STOP 32
    IF ( ANY (As%GetID()  .NE. RESHAPE((/-2,-2,-2,-2/), (/2,2/)) ) ) STOP 33
    IF ( ANY (As%ChildID  .NE. RESHAPE((/-2,-2,-2,-2/), (/2,2/)) ) ) STOP 34
    IF ( ANY (As%BaseID        .NE. RESHAPE((/-1,-1,-1,-1/), (/2,2/)) ) ) STOP 35
    IF ( ANY (As%Base%BaseID   .NE. RESHAPE((/-1,-1,-1,-1/), (/2,2/)) ) ) STOP 36
    IF ( LEN(As%C)        .NE. 1023)                   STOP 37
    IF ( TRIM(As(1,1)%C)  .NE. "!" )                   STOP 38

    ASSOCIATE( As => SPREAD(As(1,:), 1, 2) )

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


