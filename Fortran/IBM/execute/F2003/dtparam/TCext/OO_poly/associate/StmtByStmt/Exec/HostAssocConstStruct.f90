! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/associate/StmtByStmt/Exec/HostAssocConstStruct.f
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP:  HostAssocConstStruct.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : HostAssocConstStruct
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
!*    The selector is an associte name associating to a constant structure (component)
!*   (ICE)
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

    FUNCTION GetChildId(Arg)
    CLASS(Child(4)) :: Arg
    INTEGER      :: GetChildId
      GetChildId = Arg%ChildId
    END FUNCTION

    FUNCTION GetBaseId(Arg)
    CLASS(Base(4))  :: Arg
    INTEGER      :: GetBaseId
      GetBaseId = Arg%BaseId
    END FUNCTION

    SUBROUTINE SetBaseId(Arg)
    CLASS(Base(4))  :: Arg
      Arg%BaseId = -1
    END SUBROUTINE

  END MODULE

  PROGRAM HostAssocConstStruct
  USE M
  IMPLICIT NONE

  TYPE(Base(4)),  PARAMETER :: V = Base(4)()
  TYPE(Child(4)), PARAMETER :: W = Child(4)()

    ASSOCIATE ( T0 => W, T1 => W%Base, T2 => V )
    ASSOCIATE ( As0 => T0, As1 => T1, As2 => T2)

      IF ( As0%GetID()      .NE. 2 ) STOP 41
      IF ( As0%ChildID      .NE. 2 ) STOP 42
      IF ( As0%Base%GetId() .NE. 1 ) STOP 43
      IF ( As0%BaseId       .NE. 1 ) STOP 44

      IF ( As1%BaseID  .NE. 1 ) STOP 50
      IF ( As1%GetID() .NE. 1 ) STOP 51

      IF ( As2%GetId() .NE. 1 ) STOP 52
      IF ( As2%BaseId  .NE. 1 ) STOP 53

    END ASSOCIATE
    END ASSOCIATE

  END
