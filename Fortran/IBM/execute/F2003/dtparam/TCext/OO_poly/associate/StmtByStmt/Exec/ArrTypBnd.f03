! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/associate/StmtByStmt/Exec/ArrTypBnd.f
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
!*    The selector is a type bound function call
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE :: Base(K1)    ! (4)
      INTEGER, KIND :: K1
      INTEGER(K1)   :: BaseId = 1
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetBaseId
      PROCEDURE, NoPASS :: SetId
      PROCEDURE, PASS   :: GetObj
    END TYPE

    TYPE, EXTENDS(Base) :: Child    ! (4)
      INTEGER(K1)  :: ChildId = 2
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetChildId
    END TYPE

    CONTAINS

    ELEMENTAL FUNCTION GetChildId(Arg)
    CLASS(Child(4)), INTENT(IN) :: Arg
    INTEGER      :: GetChildId
      GetChildId = Arg%ChildId
    END FUNCTION

    ELEMENTAL FUNCTION GetBaseId(Arg)
    CLASS(Base(4)), INTENT(IN) :: Arg
    INTEGER      :: GetBaseId
      GetBaseId = Arg%BaseId
    END FUNCTION

    ELEMENTAL FUNCTION GetObj(Arg)
    CLASS(Base(4)), INTENT(IN) :: Arg
    TYPE (Child(4))    :: GetObj
      SELECT TYPE (Arg)
      TYPE IS (Child(4))
        GetObj = Arg
      TYPE IS (Base(4))
        GetObj%Base = Arg
      CLASS DEFAULT
      END SELECT
    END FUNCTION

    SUBROUTINE SetId(Obj, Id)
    CLASS(Base(4))  :: Obj(:,:)
    INTEGER      :: Id
      SELECT TYPE (Obj)
      TYPE IS (Base(4))
        Obj%BaseId = Id
      TYPE IS (Child(4))
        Obj%ChildId = Id
      END SELECT
    END SUBROUTINE

  END MODULE

  PROGRAM ArrTypBnd
  USE M
  IMPLICIT NONE
  INTEGER :: i
  CLASS(*), ALLOCATABLE :: Arr(:,:)

  ALLOCATE(Arr(5,5), SOURCE=Child(4)(BaseID=-1, ChildID=-2))

  ASSOCIATE ( As => Arr(2:3, 4:5) )
  SELECT TYPE (As )
  CLASS IS ( Child(4))
  ASSOCIATE ( As => As(:,:)%GetObj() )

    IF ( ANY (LBOUND(As)  .NE. (/1,1/) ) )             ERROR STOP 30
    IF ( ANY (UBOUND(As)  .NE. (/2,2/) ) )             ERROR STOP 30
    IF ( ANY (SHAPE(As)   .NE. (/2,2/) ) )             ERROR STOP 32

    IF ( ANY (As%Base%GetID()  .NE. RESHAPE((/-1,-1,-1,-1/), (/2,2/)) ) ) ERROR STOP 33
    IF ( ANY (As%GetID()       .NE. RESHAPE((/-2,-2,-2,-2/), (/2,2/)) ) ) ERROR STOP 34

    ASSOCIATE ( As0 => As%ChildId, As1 => As%BaseId )
       IF ( ANY(As0 .NE. RESHAPE((/-2,-2,-2,-2/), (/2,2/)) ) ) ERROR STOP 41
       IF ( ANY(As1 .NE. RESHAPE((/-1,-1,-1,-1/), (/2,2/)) ) ) ERROR STOP 42
    END ASSOCIATE

    ASSOCIATE ( As2 => As%Base )
      IF ( ANY(As2%GetID() .NE. RESHAPE((/-1,-1,-1,-1/), (/2,2/)) )) ERROR STOP 50
    END ASSOCIATE

    ASSOCIATE (As =>  As%GetID())
      IF ( ANY(As .NE. RESHAPE((/-2,-2,-2,-2/), (/2,2/)) )) ERROR STOP 60
    END ASSOCIATE

    ASSOCIATE (As =>  As%Base%GetID())
      IF ( ANY(As .NE. RESHAPE((/-1,-1,-1,-1/), (/2,2/)) )) ERROR STOP 70
    END ASSOCIATE

    ASSOCIATE (As =>  As%Base%GetObj())
      IF ( ANY(As%BaseId       .NE. RESHAPE((/-1,-1,-1,-1/), (/2,2/)) )) ERROR STOP 71
      IF ( ANY(As%Base%GetID() .NE. RESHAPE((/-1,-1,-1,-1/), (/2,2/)) )) ERROR STOP 72
    END ASSOCIATE

    CALL As%SetId(As%Base, 1)
    CALL As%SetId(As, 2)

    IF ( ANY (As%GetID()      .NE. RESHAPE((/2,2,2,2/), (/2,2/)) ) ) ERROR STOP 83
    IF ( ANY (As%Base%GetID() .NE. RESHAPE((/1,1,1,1/), (/2,2/)) ) ) ERROR STOP 84

  END ASSOCIATE
  CLASS DEFAULT
    STOP 99
  END SELECT
  END ASSOCIATE

  END
