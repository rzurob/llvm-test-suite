! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP:  HostAssocConstStructArr.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : HostAssocConstStructArr
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

    TYPE :: Base
      INTEGER :: BaseId = 1
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetBaseId
      PROCEDURE, PASS   :: SetId => SetBaseId
    END TYPE

    TYPE, EXTENDS(Base) :: Child
      INTEGER  :: ChildId = 2
      CLASS(Base), POINTER :: BasePtr => NULL()
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetChildId
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
      Arg%BaseId = -1
    END SUBROUTINE

  END MODULE

  PROGRAM HostAssocConstStructiArr
  USE M
  IMPLICIT NONE

  INTEGER                :: i
  TYPE(Base),  PARAMETER :: V(3) = (/(Base(i), i =1, 3)/)
  TYPE(Child), PARAMETER :: W(4) = (/ (Child(BaseId=i, ChildId=-i), i=1, 4) /)

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
