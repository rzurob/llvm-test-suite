!*********************************************************************
!*  ===================================================================
!*
!*  TESTOP CASE NAME             : InitExpPrecedOP.f
!*  TESTOP CASE TITLE            :
!*
!*  DATE                       : Sept. 07 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Fortran 2003 Initialization Expression Enhancement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature Charber 289074
!*
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Precedence of operators
!*
!234567890123456789012345678901234567890123456789012345678901234567890




  PROGRAM InitExpPrecedOP
  IMPLICIT NONE

  INTEGER     :: I

  REAL, PARAMETER :: A(128)= 1
  REAL, PARAMETER :: B(128)= 2

  INTEGER, PARAMETER :: C(128)= 1
  INTEGER, PARAMETER :: D(128)= 2

 !INTEGER, PARAMETER :: A(128)= 1
 !INTEGER, PARAMETER :: B(128)= 2

  INTEGER :: IExp(128)      = -A**B
  REAL    :: RExpMul(128)   = B*A**B
  REAL    :: RMulUnary(128) = -[(-0., I=1,128)]*[(-0., I=1,128)]
  REAL    :: RUnarySub(128) = -A-B
  LOGICAL :: LConcatRel(128)= CHAR(C)//CHAR(D) < CHAR(C)
  LOGICAL :: LRelNot(128)   = .NOT. C < D



  IF ( ANY( IExp                .NE. -1   ) ) ERROR STOP 11
  IF ( ANY( RExpMul             .NE.  2   ) ) ERROR STOP 12
  IF ( ANY( SIGN(1.,RMulUnary)  .NE. -1   ) ) ERROR STOP 13
  IF ( ANY( RUnarySub           .NE. -3   ) ) ERROR STOP 14

  IF ( ANY( .NOT. LConcatRel    ) ) ERROR STOP 15
  IF ( ANY( LRelNot             ) ) ERROR STOP 16




  END


