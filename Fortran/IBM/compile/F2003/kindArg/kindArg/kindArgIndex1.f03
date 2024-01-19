!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jun. 15, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : New Kind argumnet for existing intrinsics
!*
!*  SECONDARY FUNCTIONS TESTED : INDEX
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


  PROGRAM kindArgIndex1

  INTEGER,    PARAMETER :: I(3) = (/-1,0,3/)
  INTEGER,    PARAMETER :: J(3) = (/1,1,1/)


  PRINT*, INDEX(STRING=CHAR(1_1), SUBSTRING=CHAR(1_1), KIND=I(1))
  PRINT*, INDEX(STRING=CHAR(1_1), SUBSTRING=CHAR(1_1), KIND=I(2))
  PRINT*, INDEX(KIND=I(3),        SUBSTRING=CHAR(1_1), STRING=CHAR(1_1))

  PRINT*, INDEX((/"A","B","C"/), (/"A","B","C"/), KIND=7)

  PRINT*, INDEX(KIND=J(J(J(1))), STRING=(/"A","B","C"/), SUBSTRING=(/"A","B","C"/)) !ok

  END

