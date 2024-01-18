! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/associate/StmtByStmt/Exec/ArrSection2.f
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP:  ArrSection2.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : ArrSection2
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
!*    The selector is a poly array section
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

  PROGRAM ArrSection2
  USE M
  IMPLICIT NONE

  CLASS(*), ALLOCATABLE :: Arr(:)

  ALLOCATE(Child(4,20) :: Arr(10))

  ASSOCIATE ( As => Arr(::2) )
  ASSOCIATE ( As => as(::2) )
  SELECT TYPE ( As )
  TYPE IS (Child(4,*))
    As%BaseId = -1
    As%ChildId = -2

    IF ( ANY (LBOUND(As)      .NE. (/1/) ) )             STOP 30
    IF ( ANY (SHAPE(As)       .NE. (/3/) ) )             STOP 32
    IF ( ANY (As%GetID()      .NE. (/-2,-2,-2/) ) ) STOP 33
    IF ( ANY (As%Base%GetID() .NE. (/-1,-1,-1/) ) ) STOP 34

    ASSOCIATE ( As0 => As%ChildId, As1 => As%BaseId )
       IF ( ANY(As0 .NE. (/-2,-2,-2/) ) ) STOP 41
       IF ( ANY(As1 .NE. (/-1,-1,-1/) ) ) STOP 42
    END ASSOCIATE

    ASSOCIATE ( As2 => As%Base )
      IF ( ANY(As2%GetID() .NE. (/-1,-1,-1/) )) STOP 50
    END ASSOCIATE

    ASSOCIATE (As1 =>  As%GetID())
      IF ( ANY(As1 .NE. (/-2,-2,-2/) )) STOP 60
    END ASSOCIATE

    ASSOCIATE (As1 =>  As%Base%GetID())
      IF ( ANY(As1 .NE. (/-1,-1,-1/) )) STOP 70
    END ASSOCIATE

  CLASS DEFAULT
    STOP 80
  END SELECT
  END ASSOCIATE
  END ASSOCIATE

  END
