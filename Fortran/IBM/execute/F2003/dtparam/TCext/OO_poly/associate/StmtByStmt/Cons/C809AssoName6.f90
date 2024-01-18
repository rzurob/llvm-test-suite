! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/associate/StmtByStmt/Cons/C809AssoName6.f
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
!*     Selector is an array constructor with the same name as associate name
!*    (ICE)
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

    ELEMENTAL FUNCTION GetChildId(Arg)
    CLASS(Child(4,*)), INTENT(IN) :: Arg
    INTEGER      :: GetChildId
      GetChildId = Arg%Id
    END FUNCTION

  END MODULE

  PROGRAM C809AssoName6
  USE M
  IMPLICIT NONE
  integer i

    ASSOCIATE ( Child => (/(Child(4,20)(i), i =1, 3) /) )
      IF ( ANY(SHAPE(Child)  .NE. (/3/)) )    STOP 50
      IF ( ANY(Child%GetId() .NE. (/1,2,3/))) STOP 51

      ASSOCIATE ( Child => Child  )
        IF ( ANY(SHAPE(Child)  .NE. (/3/)) )    STOP 52
        IF ( ANY(Child%Id      .NE. (/1,2,3/))) STOP 53
        IF ( ANY(Child%GetId() .NE. (/1,2,3/))) STOP 54
      END ASSOCIATE
    END ASSOCIATE

  END

