! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/associate/StmtByStmt/Exec/ConstStruct.f
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
!*    The selector is a constant structure (component)
!*   (Coredump)
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
      PROCEDURE, NOPASS :: PrintType => PrintChild
      PROCEDURE, PASS   :: GetId => GetChildId
    END TYPE

    CONTAINS

    SUBROUTINE PrintChild()
      PRINT *,'Child'
    END SUBROUTINE

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

  PROGRAM ConstStruct
  USE M
  IMPLICIT NONE

  TYPE(Child(4)) :: V = Child(4)()
  TYPE(Child(4)), PARAMETER :: W = Child(4)()

    ASSOCIATE ( As => Child(4)() )
      IF ( As%GetID() .NE. 2) ERROR STOP 50
      ASSOCIATE ( As0 => As%ChildId, As1 => As%BaseId )
         IF ( As0 .NE. 2) ERROR STOP 51
         IF ( As1 .NE. 1) ERROR STOP 52
      END ASSOCIATE

      ASSOCIATE ( As2 => As%Base )
        IF ( As2%BaseID .NE. 1 ) ERROR STOP 53
      END ASSOCIATE
    END ASSOCIATE

    ASSOCIATE ( As => V )
      IF ( As%GetID() .NE. 2) ERROR STOP 54
      ASSOCIATE ( As0 => As%ChildId, As1 => As%BaseId )
         IF ( As0 .NE. 2) ERROR STOP 55
         IF ( As1 .NE. 1) ERROR STOP 56
      END ASSOCIATE

      ASSOCIATE ( As => As%Base )
        IF ( As%GetID() .NE. 1 ) ERROR STOP 57
      END ASSOCIATE

      IF( As%GetID() .NE. 2 ) ERROR STOP 57
    END ASSOCIATE

    ASSOCIATE ( As => W )
      IF ( As%GetID() .NE. 2) ERROR STOP 58
      ASSOCIATE ( As0 => As%ChildId, As1 => As%BaseId )
         IF ( As0 .NE. 2) ERROR STOP 59
         IF ( As1 .NE. 1) ERROR STOP 60
      END ASSOCIATE

      ASSOCIATE ( As => As%Base )
        IF ( As%GetId() .NE. 1 ) ERROR STOP 61
      END ASSOCIATE

      ASSOCIATE ( As => W%Base%GetID() )
        IF ( As .NE. 1 ) ERROR STOP 62
      END ASSOCIATE
    END ASSOCIATE

  END
