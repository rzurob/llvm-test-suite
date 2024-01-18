! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/associate/StmtByStmt/Exec/HostAssocVarPolyAlloc.f
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP:  HostAssocVarPolyAlloc.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : HostAssocVarPolyAlloc
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
!*    The selector is an associate name associating to a poly allocatable variable of derived types
!*    (Wrong result: stop 31)
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
      INTEGER(K1)    :: ChildId = 2
      TYPE(Base(K1)) :: BaseComp
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

  PROGRAM HostAssocVarPolyAlloc
  USE M
  IMPLICIT NONE

  CLASS(Base(4)), ALLOCATABLE   :: U

  ALLOCATE(U, SOURCE=Child(4)(BaseId=-1, ChildId=-2, BaseComp=Base(4)(0)))

  ASSOCIATE ( As => U )
  ASSOCIATE ( As => As )

    SELECT TYPE ( As )
      TYPE IS ( Child(4) )
        IF ( As%GetId()      .NE. -2 ) STOP 30
        IF ( As%ChildId      .NE. -2 ) STOP 31
        IF ( As%Base%GetId() .NE. -1 ) STOP 32
        IF ( As%BaseComp%GetId() .NE. 0 ) STOP 33

        As%BaseId  = 1
        As%ChildId = 2

        IF ( As%GetId()      .NE. 2 ) STOP 40
        IF ( As%ChildId      .NE. 2 ) STOP 41
        IF ( As%Base%GetId() .NE. 1 ) STOP 42
        IF ( U%GetId()       .NE. 2 ) STOP 43
      CLASS DEFAULT
        STOP 50
    END SELECT
  END ASSOCIATE
  END ASSOCIATE

  END
