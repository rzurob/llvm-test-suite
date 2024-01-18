!*********************************************************************
!*  ===================================================================
!*
!*  TESTOP1 CASE NAME             : InitExpPrecedOP1.f
!*  TESTOP1 CASE TITLE            :
!*
!*  DATE                       : Sept. 08 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Fortran 2003 Initialization Expression Enhancement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature Charber 289074
!*
!*  REQUIRED COMPILER OP1TIONS  : -qfree=f90
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




  PROGRAM InitExpPrecedOP1
  IMPLICIT NONE

  INTEGER     :: I

  REAL(4),  PARAMETER :: A(128)= 1
  REAL(8),  PARAMETER :: B(128)= 2
  REAL(16), PARAMETER :: C(128)= 2



  INTEGER :: IDiv(128)      = INT(A)/INT(B)/INT(C)
  REAL    :: IExp(128)      = B**A**C


  IF ( ANY( IDiv    .NE. 0  ) ) ERROR STOP 11
  IF ( ANY( IExp    .NE. 2  ) ) ERROR STOP 12


  END



