! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/associate/StmtByStmt/Cons/C808Str2.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Oct. 20, 2004
!*
!*  PRIMARY FUNCTIONS TESTED   : Associate
!*
!*  SECONDARY FUNCTIONS TESTED : Selector is a constant
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
!*    The selector is a structure constructor with abstract parent component
!*    (Pass Exce)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM C808Arr2
  IMPLICIT NONE

    TYPE, ABSTRACT :: Base(K1)    ! (4)
      INTEGER, KIND :: K1
      INTEGER(K1)   :: Id = 0
    END TYPE

    TYPE, EXTENDS(Base) :: Child    ! (4)
    END TYPE

    ASSOCIATE ( As => Child(4)(Id = 1) )
      As%Base%Id = 5

      ASSOCIATE ( As => As%Base )
      END ASSOCIATE

    END ASSOCIATE


  END
