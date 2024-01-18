! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/associate/StmtByStmt/Cons/C808Misc2.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Oct. 26, 2004
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
!*    The selector is a constant object of derived type
!*    (Comp failed-syntax err)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

    TYPE :: Base(K1)    ! (4)
      INTEGER, KIND :: K1
      INTEGER(K1)   :: Id = 1
    CONTAINS
      PROCEDURE, PASS :: GetId => getbaseid
    END TYPE

  CONTAINS

    SUBROUTINE printbase()
      PRINT *,'base'
    END SUBROUTINE

    FUNCTION getbaseid(A)
      CLASS(Base(4)), INTENT(IN) :: A
      INTEGER :: getbaseid
      getbaseid = a%id
    END FUNCTION

  END MODULE

  PROGRAM C808Misc2
  USE M
  IMPLICIT NONE

    TYPE(Base(4)), PARAMETER :: V = Base(4)(1)

    ASSOCIATE ( As => V)
      IF ( As%GetId() .NE. 1 ) ERROR STOP 11
    END ASSOCIATE

  END
