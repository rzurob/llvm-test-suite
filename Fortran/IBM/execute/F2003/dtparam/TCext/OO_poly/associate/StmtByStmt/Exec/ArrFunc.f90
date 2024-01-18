! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/associate/StmtByStmt/Exec/ArrFunc.f
! *********************************************************************
!*  ===================================================================
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
!*    The selector is a function return  of array
!*    ()
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

  END MODULE

  PROGRAM ArrFunc
  USE M
  IMPLICIT NONE
  INTEGER :: i
  CLASS(Base(4,1,:)), ALLOCATABLE :: Arr(:,:)
  INTEGER(8) :: S1(2)=(/5,4/), S2(2)=(/1,1/)

  ALLOCATE(Arr(5, 5), SOURCE=Child(4,1,1023)(BaseID=-1, ChildID=-2))

  ASSOCIATE ( As => Arr(S1, S2) )

    IF ( ANY (LBOUND(As)  .NE. (/1,1/) ) )             STOP 30
    IF ( ANY (UBOUND(As)  .NE. (/2,2/) ) )             STOP 31
    IF ( ANY (SHAPE(As)   .NE. (/2,2/) ) )             STOP 32
    IF ( ANY (As%GetID()  .NE. RESHAPE((/-2,-2,-2,-2/), (/2,2/)) ) ) STOP 33
    IF ( ANY (As%BaseID   .NE. RESHAPE((/-1,-1,-1,-1/), (/2,2/)) ) ) STOP 36
    IF ( LEN(As%C)        .NE. 1023)                   STOP 34
    IF ( TRIM(As(1,1)%C)  .NE. "!" )                   STOP 35

  SELECT TYPE (As => As(:,:))
  CLASS IS ( Child(4,1,*))

    IF ( ANY (LBOUND(As)  .NE. (/1,1/) ) )             STOP 40
    IF ( ANY (UBOUND(As)  .NE. (/2,2/) ) )             STOP 41
    IF ( ANY (SHAPE(As)   .NE. (/2,2/) ) )             STOP 42
    IF ( ANY (As%GetID()       .NE. RESHAPE((/-2,-2,-2,-2/), (/2,2/)) ) ) STOP 43
    IF ( ANY (As%Base%GetID()  .NE. RESHAPE((/-1,-1,-1,-1/), (/2,2/)) ) ) STOP 46
    IF ( ANY (As%ChildID       .NE. RESHAPE((/-2,-2,-2,-2/), (/2,2/)) ) ) STOP 47
    IF ( ANY (As%BaseID        .NE. RESHAPE((/-1,-1,-1,-1/), (/2,2/)) ) ) STOP 48
    IF ( LEN(As%C)        .NE. 1023)                   STOP 44
    IF ( TRIM(As(2,2)%C)  .NE. "!" )                   STOP 45

  CLASS DEFAULT
    STOP 99
  END SELECT
  END ASSOCIATE

  END


