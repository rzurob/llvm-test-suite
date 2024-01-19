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
!*   -Nullify
!*    (300325)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM VarDef2

  TYPE :: DT
    INTEGER, POINTER :: IntP
  END TYPE

  INTEGER, TARGET :: Tar = 1

  ASSOCIATE ( As => DT(Tar) )

    PRINT*, ASSOCIATED(As%IntP)
    NULLIFY ( As%IntP )
    PRINT*, ASSOCIATED(As%IntP)

  END ASSOCIATE


  END

