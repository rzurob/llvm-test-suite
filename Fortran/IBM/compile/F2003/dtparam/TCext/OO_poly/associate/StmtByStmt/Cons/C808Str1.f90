! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/associate/StmtByStmt/Cons/C808Str1.f
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
!*    The selector is a structure constructor with array component
!*    (Pass Exce)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM C808Str1
  IMPLICIT NONE

  TYPE :: T(K1)    ! (4)
    INTEGER, KIND :: K1
    INTEGER(K1)   :: Arr(3)
  END TYPE

    ASSOCIATE ( As => T(4)((/1, 2, 3/)) )
      As%Arr(1) = 4
      print*, As%Arr(1)
    END ASSOCIATE

  END
