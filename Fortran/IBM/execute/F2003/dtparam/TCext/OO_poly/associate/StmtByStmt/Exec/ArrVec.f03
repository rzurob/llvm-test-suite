! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/associate/StmtByStmt/Exec/ArrVec.f
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
!*    The selector is an array section with a vector subscript
!*    (ICE)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE Zero(K1,N1)    ! (4,20)
        INTEGER, KIND :: K1
        INTEGER, LEN  :: N1
    END TYPE

    TYPE, EXTENDS(Zero) :: Base    ! (4,20)
      INTEGER(K1) :: BaseId = 1
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetBaseId
    END TYPE

    TYPE, EXTENDS(Base) :: Child    ! (4,20)
      INTEGER(K1)  :: ChildId = 2
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetChildId
    END TYPE

    CONTAINS

    ELEMENTAL FUNCTION GetChildId(Arg)
    CLASS(Child(4,*)), INTENT(IN) :: Arg
    INTEGER      :: GetChildId
      GetChildId = Arg%ChildId
    END FUNCTION

    ELEMENTAL FUNCTION GetBaseId(Arg)
    CLASS(Base(4,*)), INTENT(IN) :: Arg
    INTEGER      :: GetBaseId
      GetBaseId = Arg%BaseId
    END FUNCTION

  END MODULE

  PROGRAM ArrVec
  USE M
  IMPLICIT NONE

  CLASS(Child(4,:)), POINTER :: Arr(:)
  INTEGER :: i

  ALLOCATE( Arr(10), SOURCE=(/(Child(4,20)(BaseId=i, ChildId=-i), i=1,10)/) )

  ASSOCIATE ( As => Arr((/2,4,6,8,10/)))
  ASSOCIATE ( As => as((/4,1,3,2/)) )

    IF ( ANY (LBOUND(As)      .NE. (/1/) ) )             ERROR STOP 30
    IF ( ANY (SHAPE(As)       .NE. (/4/) ) )             ERROR STOP 32
    IF ( ANY (As%GetID()      .NE. (/-8,-2,-6,-4/) ) ) ERROR STOP 33
    IF ( ANY (As%Base%GetID() .NE. (/ 8, 2, 6, 4/) ) ) ERROR STOP 34

    ASSOCIATE ( As0 => As%ChildId, As1 => As%BaseId )
       IF ( ANY(As0 .NE. (/-8,-2,-6,-4/) ) ) ERROR STOP 41
       IF ( ANY(As1 .NE. (/ 8, 2, 6, 4/) ) ) ERROR STOP 42
    END ASSOCIATE

    ASSOCIATE ( As2 => As%Base )
      IF ( ANY(As2%GetID() .NE. (/ 8, 2, 6, 4/) )) ERROR STOP 50
    END ASSOCIATE

    ASSOCIATE (As1 =>  As%GetID())
      IF ( ANY(As1 .NE. (/-8,-2,-6,-4/) )) ERROR STOP 60
    END ASSOCIATE

    ASSOCIATE (As1 =>  As%Base%GetID())
      IF ( ANY(As1 .NE. (/ 8, 2, 6, 4/) )) ERROR STOP 70
    END ASSOCIATE

  END ASSOCIATE
  END ASSOCIATE

  END