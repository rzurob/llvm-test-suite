! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/associate/StmtByStmt/Exec/HostAssocConstStructArr.f
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
!*    The selector is an associte name associating to a constant array of derived
!*   ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE :: Base(K1)    ! (4)
      INTEGER, KIND :: K1
      INTEGER(K1)   :: BaseId = 1
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetBaseId
      PROCEDURE, PASS   :: SetId => SetBaseId
    END TYPE

    TYPE, EXTENDS(Base) :: Child    ! (4)
      INTEGER(K1)  :: ChildId = 2
      CLASS(Base(K1)), POINTER :: BasePtr => NULL()
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetChildId
    END TYPE

    CONTAINS

    ELEMENTAL FUNCTION GetChildId(Arg)
    CLASS(Child(4)), INTENT(IN) :: Arg
    INTEGER                  :: GetChildId
      GetChildId = Arg%ChildId
    END FUNCTION

    ELEMENTAL FUNCTION GetBaseId(Arg)
    CLASS(Base(4)), INTENT(IN)  :: Arg
    INTEGER                  :: GetBaseId
      GetBaseId = Arg%BaseId
    END FUNCTION

    SUBROUTINE SetBaseId(Arg)
    CLASS(Base(4))  :: Arg
      Arg%BaseId = -1
    END SUBROUTINE

  END MODULE

  PROGRAM HostAssocConstStructiArr
  USE M
  IMPLICIT NONE

  INTEGER                :: i
  TYPE(Base(4)),  PARAMETER :: V(3) = (/(Base(4)(i), i =1, 3)/)
  TYPE(Child(4)), PARAMETER :: W(4) = (/ (Child(4)(BaseId=i, ChildId=-i), i=1, 4) /)

    ASSOCIATE ( T0 => V, T1 => V(1:3:2), T2 => V((/2,1,2/)))
    ASSOCIATE ( T3 => W, T4 => W%BaseId, T5 => W%Base)
    ASSOCIATE ( As0 => T0, As1 => T1, As2 => T2)
    ASSOCIATE ( As3 => T3, As4 => T4, As5 => T5)

      IF ( ANY( As0%BaseID  .NE. (/1,2,3 /) ) ) STOP 40
      IF ( ANY( As0%GetID() .NE. (/1,2,3 /) ) ) STOP 41
      IF ( ANY( SHAPE(As0)  .NE. (/3/)) )       STOP 42

      IF ( ANY( As1%BaseID  .NE. (/1,3 /) ) ) STOP 50
      IF ( ANY( As1%GetID() .NE. (/1,3 /) ) ) STOP 51
      IF ( ANY( SHAPE(As1)  .NE. (/2/)) )     STOP 52

      IF ( ANY( As2%BaseID  .NE. (/2,1,2 /) ) ) STOP 60
      IF ( ANY( As2%GetID() .NE. (/2,1,2 /) ) ) STOP 61
      IF ( ANY( SHAPE(As2)  .NE. (/3/)) )       STOP 62

      IF ( ANY( As3%BaseID          .NE. (/1,2,3,4 /) ) )     STOP 70
      IF ( ANY( As3%ChildID         .NE. (/-1,-2,-3,-4 /) ) ) STOP 71
      IF ( ANY( As3%GetID()         .NE. (/-1,-2,-3,-4 /) ) ) STOP 72
      IF ( ANY( As3%Base%GetID()    .NE. (/1,2,3,4 /) ) )     STOP 73
      IF ( ANY( SHAPE(As3)          .NE. (/4/)) )             STOP 74

      IF ( ANY( As4 .NE. (/1,2,3,4 /) ) )     STOP 80

      IF ( ANY( As5%BaseID          .NE. (/1,2,3,4 /) ) ) STOP 90
      IF ( ANY( As5%GetID()         .NE. (/1,2,3,4 /) ) ) STOP 91
      IF ( ANY( SHAPE(As5)          .NE. (/4/)) )         STOP 92

    END ASSOCIATE
    END ASSOCIATE
    END ASSOCIATE
    END ASSOCIATE

  END
