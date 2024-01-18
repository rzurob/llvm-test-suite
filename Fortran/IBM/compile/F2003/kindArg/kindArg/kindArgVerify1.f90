!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : kindArgVerify1
!*
!*  DATE                       : Jul. 06, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : New Kind argumnet for existing intrinsics
!*
!*  SECONDARY FUNCTIONS TESTED : VERIFY
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


  PROGRAM kindArgVerify1

  INTEGER,    PARAMETER :: I(3) = (/-1,0,3/)
  INTEGER,    PARAMETER :: J(3) = (/1,1,1/)


  PRINT*, VERIFY(STRING=CHAR(1_1), SET=CHAR(1_1), KIND=I(1))
  PRINT*, VERIFY(STRING=CHAR(1_1), SET=CHAR(1_1), KIND=I(2))
  PRINT*, VERIFY(KIND=I(3),        SET=CHAR(1_1), STRING=CHAR(1_1))

  PRINT*, VERIFY((/"A","B","C"/), (/"A","B","C"/), KIND=7)

  PRINT*, VERIFY(KIND=J(J(J(1))), STRING=(/"A","B","C"/), SET=(/"A","B","C"/)) !ok

  END

