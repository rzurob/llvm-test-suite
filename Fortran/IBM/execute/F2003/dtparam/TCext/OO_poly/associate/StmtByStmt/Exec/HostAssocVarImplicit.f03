! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/associate/StmtByStmt/Exec/HostAssocVarImplicit.f
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
!*    The selector is an associate name associating to a non-poly variable of implied type
!*    (Comp failed)
!*  (This time the init values of componets are wrong)
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
      TYPE(Base(K1)) :: BaseComp = Base(K1)(0)
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

  PROGRAM HostAssocVarImplicit
  USE M
  IMPLICIT TYPE(Child(4))(A, U)

  ASSOCIATE ( T1 => U, T2 => U  )

    IF ( As%GetID()   .NE. 2) ERROR STOP 40
    IF ( As%ChildID   .NE. 2) ERROR STOP 41
    IF ( As%BaseID    .NE. 1) ERROR STOP 42

    U%BaseId  = -1
    U%ChildId = -2

    IF ( T2%GetID()   .NE. -2) ERROR STOP 50
    IF ( T2%ChildID   .NE. -2) ERROR STOP 51
    IF ( T2%BaseID    .NE. -1) ERROR STOP 52

    ASSOCIATE ( As1 => T1, As2 => T1 )
      IF ( As1%GetID()   .NE. -2) ERROR STOP 60
      IF ( As2%ChildID   .NE. -2) ERROR STOP 61
      IF ( As2%BaseID    .NE. -1) ERROR STOP 62

      As2%BaseId  = 1
      As2%ChildId = 2

      IF ( U%GetID()   .NE. 2) ERROR STOP 70
      IF ( U%ChildID   .NE. 2) ERROR STOP 71
      IF ( U%BaseID    .NE. 1) ERROR STOP 72
    END ASSOCIATE

  END ASSOCIATE

  END
