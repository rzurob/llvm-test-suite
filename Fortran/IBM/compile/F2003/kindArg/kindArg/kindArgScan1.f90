!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : kindArgScan1
!*
!*  DATE                       : Jun. 28, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : New Kind argumnet for existing intrinsics
!*
!*  SECONDARY FUNCTIONS TESTED : SCAN
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


  PROGRAM kindArgScan1

  INTEGER,    PARAMETER :: I(3) = (/-1,0,3/)
  INTEGER,    PARAMETER :: J(3) = (/1,1,1/)


  PRINT*, SCAN(STRING=CHAR(1_1), SET=CHAR(1_1), KIND=I(1))
  PRINT*, SCAN(STRING=CHAR(1_1), SET=CHAR(1_2), KIND=I(2))
  PRINT*, SCAN(KIND=I(3),        SET=CHAR(1_8), STRING=CHAR(1_4))

  PRINT*, SCAN((/"A","B","C"/), (/"A","B","C"/), KIND=7)

  PRINT*, SCAN(KIND=SUM(I), STRING=(/"A","B","C"/), SET=(/"A","B","C"/)) !ok

  END

