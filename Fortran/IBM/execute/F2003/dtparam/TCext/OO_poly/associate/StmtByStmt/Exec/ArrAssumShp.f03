! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/associate/StmtByStmt/Exec/ArrAssumShp.f
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
!*    The selector an assumed shpe of array
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE :: Base(K1)    ! (4)
      INTEGER, KIND :: K1
      INTEGER(K1)   :: BaseId = 1
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetBaseId
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

  END MODULE

  PROGRAM ArrAssumShp
  USE M
  IMPLICIT NONE
  INTEGER :: i

  CALL Sub(RESHAPE((/(Child(4)(BaseId=-1, ChildId=-2), i=1,4)/), (/2,2/)) )

  CONTAINS

  SUBROUTINE Sub(Arg)
  CLASS(*) :: Arg(3:,3:)

  SELECT TYPE (Arg)
  TYPE IS (Child(4))
  ASSOCIATE ( As => Arg )

    IF ( ANY (LBOUND(As)      .NE. (/3,3/) ) )             ERROR STOP 30
    IF ( ANY (SHAPE(As)       .NE. (/2,2/) ) )             ERROR STOP 32
print*, As%GetID()
    IF ( ANY (As%GetID()      .NE. RESHAPE((/-2,-2,-2,-2/), (/2,2/)) ) ) ERROR STOP 33
    IF ( ANY (As%Base%GetID() .NE. RESHAPE((/-1,-1,-1,-1/), (/2,2/)) ) ) ERROR STOP 34

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

  END ASSOCIATE
  CLASS DEFAULT
    STOP 90
  END SELECT

  END SUBROUTINE

  END
