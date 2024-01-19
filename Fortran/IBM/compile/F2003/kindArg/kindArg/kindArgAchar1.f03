!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jun. 12, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : New Kind argumnet for existing intrinsics
!*
!*  SECONDARY FUNCTIONS TESTED : ACHAR
!*
!*  REFERENCE                  : Feature Number 289083
!*
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  characteristics :: value of kind
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM kindArgAchar1

  INTEGER,    PARAMETER :: I(3) = (/-1,0,3/)

  ENUM, BIND(C)
    ENUMERATOR :: ONE=1
  END ENUM

  PRINT*, ACHAR(1_1, KIND=I(1))
  PRINT*, ACHAR(I=1_8, KIND=I(2))
  PRINT*, ACHAR(KIND=I(3), I=10_2)

  PRINT*, ACHAR(IACHAR((/"A","B","C"/)), KIND=3)

  PRINT*, ACHAR(KIND=ONE, I=40_2) !ok

  END

