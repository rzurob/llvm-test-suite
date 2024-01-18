! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/associate/StmtByStmt/Exec/VarDef4.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Feb 22, 2005
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
!*
!*   Variable Definition Context on non variable selector
!*   - do control var
!*    (300332)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM VarDef4
  IMPLICIT NONE

  TYPE :: DT(K1)    ! (4)
    INTEGER, KIND :: K1
    INTEGER(K1)   :: i
  END TYPE

  TYPE(DT(4)) :: T
  PARAMETER (T=DT(4)(0))

  ASSOCIATE ( i => T%i )

    DO i=1, 2
    END DO

  END ASSOCIATE


  END

