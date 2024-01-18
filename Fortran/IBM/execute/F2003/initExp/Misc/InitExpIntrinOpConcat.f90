!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Aug. 29, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Fortran 2003 Initialization Expression Enhancement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature Number 289074
!*
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  "//"
!*
!234567890123456789012345678901234567890123456789012345678901234567890




  PROGRAM InitExpIntrinOpConcat
  IMPLICIT NONE

  INTEGER :: I

  CHARACTER(LEN=1), PARAMETER :: C1(128)=ACHAR(0)
  CHARACTER(LEN=4), PARAMETER :: C4(128)=(/("1234", I=1, 128)/)
  CHARACTER(LEN=5), PARAMETER :: C5(128)=(/("12345", I=1, 128)/)
  CHARACTER(LEN=9), PARAMETER :: C9(128)=(/("", I=1, 128)/)

  CHARACTER(LEN=9), PARAMETER :: TC1(128)=C1 // C1
  CHARACTER(LEN=9), PARAMETER :: TC2(128)=C4 // C5
  CHARACTER(LEN=9), PARAMETER :: TC3(128)=C5 // C1 // C5
  CHARACTER(LEN=9), PARAMETER :: TC4(128)=C9(:)(1:5) // ACHAR(0) // C9(:)(7:9)

  IF ( ANY(C9   .NE. "         " ) )                   STOP 10
  IF ( ANY(TC1  .NE. ACHAR(0)//ACHAR(0)//"       " ) ) STOP 11
  IF ( ANY(TC2  .NE. "123412345" ) )                   STOP 12
  IF ( ANY(TC3  .NE. "12345"//ACHAR(0)//"123" ) )      STOP 13
  IF ( ANY(TC4  .NE. "     "//ACHAR(0)//"   " ) )      STOP 14

  END


