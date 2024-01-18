! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/associate/StmtByStmt/Cons/C809AssoName2.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Oct. 20, 2004
!*
!*  PRIMARY FUNCTIONS TESTED   : Associate
!*
!*  SECONDARY FUNCTIONS TESTED : Associate name
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
!*     The associate name must only be declared once in the ASSOCIATE statement
!*     Selector is a dummy with the same name
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE, ABSTRACT :: Base(K1,N1)    ! (4,20)
        INTEGER, KIND :: K1
        INTEGER, LEN  :: N1
    END TYPE

    TYPE, EXTENDS(Base) :: Child    ! (4,20)
      INTEGER(K1)  :: Id = 0
    CONTAINS
      PROCEDURE, NOPASS :: PrintType => PrintChild
      PROCEDURE, PASS   :: GetId => GetChildId
    END TYPE

    CONTAINS

    SUBROUTINE PrintChild()
      PRINT *,'Child'
    END SUBROUTINE

    FUNCTION GetChildId(Arg)
    CLASS(Child(4,*)) :: Arg
    INTEGER      :: GetChildId
      GetChildId = Arg%Id
    END FUNCTION

  END MODULE

  PROGRAM C809AssoName1
  USE M
  IMPLICIT NONE
  TYPE(Child(4,20)) :: V = Child(4,20)(1)

    V = F(V)

    CONTAINS

    FUNCTION F(As)
    TYPE(Child(4,*)) :: As
    TYPE(Child(4,20)) :: F

    F = Child(4,20)(1)

    ASSOCIATE ( As => As  )
      IF (As%Id .NE. 1) STOP 50
      IF (As%GetId() .NE. 1) STOP 51
      As%Id = 2

      ASSOCIATE ( As => As  )
        IF (As%Id .NE. 2) STOP 52
        IF (As%GetId() .NE. 2) STOP 53
      END ASSOCIATE
    END ASSOCIATE

    IF (As%Id .NE. 2) STOP 52
    IF (As%GetId() .NE. 2) STOP 53

    END FUNCTION

  END

