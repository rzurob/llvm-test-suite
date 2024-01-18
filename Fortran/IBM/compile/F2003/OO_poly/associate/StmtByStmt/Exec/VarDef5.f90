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
!*   - read
!*    (ICE-300345)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM VarDef5
  IMPLICIT NONE

  TYPE :: DT
    INTEGER :: i
  END TYPE

  TYPE(DT) :: T
  PARAMETER (T=DT(1))
  CHARACTER(3) :: Buf="11"

  ASSOCIATE ( j => T%i )

    READ(Buf, *) j
    PRINT*, j

  END ASSOCIATE


  END

