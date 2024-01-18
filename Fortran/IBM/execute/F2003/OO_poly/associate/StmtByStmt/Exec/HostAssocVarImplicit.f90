! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP:  HostAssocVarImplicit.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : HostAssocVarImplicit
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
!*    The selector is an associate name associating to a non-poly variable of implied type
!*    (Comp failed)
!*  (This time the init values of componets are wrong)
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE :: Base
      INTEGER :: BaseId = 1
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetBaseId
    END TYPE

    TYPE, EXTENDS(Base) :: Child
      INTEGER  :: ChildId = 2
      TYPE(Base) :: BaseComp = Base(0)
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetChildId
    END TYPE

    CONTAINS

    FUNCTION GetChildId(Arg)
    CLASS(Child) :: Arg
    INTEGER      :: GetChildId
      GetChildId = Arg%ChildId
    END FUNCTION

    FUNCTION GetBaseId(Arg)
    CLASS(Base)  :: Arg
    INTEGER      :: GetBaseId
      GetBaseId = Arg%BaseId
    END FUNCTION

  END MODULE

  PROGRAM HostAssocVarImplicit
  USE M
  IMPLICIT TYPE(Child)(A, U)

  ASSOCIATE ( T1 => U, T2 => U  )

    IF ( As%GetID()   .NE. 2) STOP 40
    IF ( As%ChildID   .NE. 2) STOP 41
    IF ( As%BaseID    .NE. 1) STOP 42

    U%BaseId  = -1
    U%ChildId = -2

    IF ( T2%GetID()   .NE. -2) STOP 50
    IF ( T2%ChildID   .NE. -2) STOP 51
    IF ( T2%BaseID    .NE. -1) STOP 52

    ASSOCIATE ( As1 => T1, As2 => T1 )
      IF ( As1%GetID()   .NE. -2) STOP 60
      IF ( As2%ChildID   .NE. -2) STOP 61
      IF ( As2%BaseID    .NE. -1) STOP 62

      As2%BaseId  = 1
      As2%ChildId = 2

      IF ( U%GetID()   .NE. 2) STOP 70
      IF ( U%ChildID   .NE. 2) STOP 71
      IF ( U%BaseID    .NE. 1) STOP 72
    END ASSOCIATE

  END ASSOCIATE

  END
