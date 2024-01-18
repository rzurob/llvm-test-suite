! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/associate/StmtByStmt/Cons/C808Str.f
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP:  redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: tcomp C808Str.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : C808Str
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
!*    The selector is an structure constructor with private componet
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M
    TYPE T(K1)    ! (4)
      INTEGER, KIND        :: K1
      INTEGER(K1), PRIVATE :: P = 1
      INTEGER(K1)          :: Q
    END TYPE

    TYPE(T(4)), SAVE :: Var

  END MODULE

  PROGRAM C808Arr
  USE M
  IMPLICIT NONE

  TYPE, EXTENDS(T) :: DT    ! (4)
  END TYPE

  TYPE(DT(4)) :: V

    ASSOCIATE ( As => Var%P )
      As%P = 1
    END ASSOCIATE

    ASSOCIATE ( As => DT(4)(Q=1) )
      As%P = 1
    END ASSOCIATE

    ASSOCIATE ( As => DT(4)(P=1, Q=2) )
      As%P = 1
    END ASSOCIATE

  END
