!*********************************************************************
!*  ===================================================================
!*
!*  TESTOP CASE NAME             : InitExpData.f
!*  TESTOP CASE TITLE            :
!*
!*  DATE                       : Sept. 11 2006
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
!*  Data Stmt  - Suggested by Jim
!*
!* (325509)
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM InitExpData
  IMPLICIT NONE

  TYPE :: DT
    INTEGER :: Subscript(0:127)
  END TYPE

  INTEGER  :: I,J
  TYPE(DT), PARAMETER :: T(0:127)=[(DT(Subscript=[(J,J=0,127)]), I=0,127)]

  INTEGER , PARAMETER :: S(0:127)=[(J,J=0,127)]

  TYPE(DT) :: T1(0:127)
  TYPE(DT) :: T2(0:127)

  DATA (T1(S(I)), I=0,127) /128*DT(Subscript=[(J,J=0,127)])/
  DATA (T2(T(I)%Subscript(I)), I=0,127) /128*DT(Subscript=[(J,J=0,127)])/

  DO I=0, 127
    IF (ANY(T1(I)%Subscript  .NE. [(J,J=0,127)] )) STOP 11
    IF (ANY(T2(I)%Subscript  .NE. [(J,J=0,127)] )) STOP 12
  END DO

  END



