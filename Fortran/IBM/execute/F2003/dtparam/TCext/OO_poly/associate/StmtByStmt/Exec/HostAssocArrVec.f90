! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/associate/StmtByStmt/Exec/HostAssocArrVec.f
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP:  HostAssocArrVec.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : HostAssocArrVec
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
!*    The selector is an associate name associating to an array with a vector subscript
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
    CLASS(Base(4)), INTENT(IN)  :: Arg
    INTEGER      :: GetBaseId
      GetBaseId = Arg%BaseId
    END FUNCTION

  END MODULE

  PROGRAM HostAssocArrVec
  USE M
  IMPLICIT NONE

  CLASS(*),    ALLOCATABLE :: U(:)

  ALLOCATE(U(3), SOURCE=Child(4)(BaseId=-1, ChildId=-2))

  ASSOCIATE ( T1 => U )
  ASSOCIATE ( T2 => T1((/3,2,1/)) )
    SELECT TYPE ( T2 )
      TYPE IS ( Child(4) )
        IF (ANY(T2%GetId() .NE. -2) )      STOP 30
        IF (ANY(T2%Base%GetId() .NE. -1) ) STOP 31
      CLASS DEFAULT
        STOP 32
    END SELECT

    ASSOCIATE ( As => T2 )
      SELECT TYPE ( As )
        TYPE IS (Child(4) )
          IF (ANY(As%GetId()      .NE. -2) ) STOP 50
          IF (ANY(As%Base%GetId() .NE. -1) ) STOP 51
        CLASS DEFAULT
          STOP 52
      END SELECT
    END ASSOCIATE

  END ASSOCIATE
  END ASSOCIATE

  DEALLOCATE(U)

  END
