! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/associate/StmtByStmt/Exec/HostAssocVarDummy.f
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
!*    The selector is an associate name associating to a nonpoly dummy variable of derived types
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
      TYPE(Base(K1)), POINTER :: BaseComp
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

  END MODULE

  PROGRAM HostAssocVarDummy
  USE M
  IMPLICIT NONE

  TYPE(Child(4)) :: V

  CALL Sub(V, V)

  CONTAINS

  SUBROUTINE Sub(Arg1, Arg2)
  IMPLICIT NONE
  TYPE(Child(4)) :: Arg1, Arg2

  ASSOCIATE ( T1 => Arg1, T2 => Arg2 )
    IF ( T1%GetId() .NE. T2%GetId() ) ERROR STOP 30
    Arg1%ChildId = -2
    IF ( T1%GetId() .NE. T2%GetId() ) ERROR STOP 31
    IF ( T1%Base%GetId() .NE. T2%Base%GetId() ) ERROR STOP 32

    ASSOCIATE ( As1 => T1, As2 => T2 )
      Arg1%BaseId  = -1
      Arg2%ChildID = -2
      IF ( As1%BaseId  .NE. -1 ) ERROR STOP 40
      IF ( As2%ChildId .NE. -2 ) ERROR STOP 41
      IF ( As1%GetId() .NE. As2%GetId() ) ERROR STOP 42
      IF ( As1%Base%GetId() .NE. As2%Base%GetId() ) ERROR STOP 43
    END ASSOCIATE

    ASSOCIATE ( As1 => T1, As2 => T2 )
      As1%BaseId  = 1
      As2%ChildID = 2
      IF ( Arg1%BaseId  .NE. 1 ) ERROR STOP 50
      IF ( Arg2%ChildId .NE. 2 ) ERROR STOP 51
      IF ( Arg1%GetId() .NE. Arg2%GetId() ) ERROR STOP 52
      IF ( Arg1%Base%GetId() .NE. Arg2%Base%GetId() ) ERROR STOP 53
    END ASSOCIATE


  END ASSOCIATE


  END SUBROUTINE

  END