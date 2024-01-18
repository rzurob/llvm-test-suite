!*********************************************************************
!*  ===================================================================
!*
!*  TESTOP CASE NAME             : InitExpAssgn3.f
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
!*  Derived type intrinsic assignment
!*
!* (325090) -- proved that no any defined assignment involved.
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  TYPE :: DT0
    INTEGER :: ID
  CONTAINS
  PROCEDURE :: MyAssgn
  GENERIC :: ASSIGNMENT(=) => MyAssgn
  END TYPE

  CONTAINS

  ELEMENTAL SUBROUTINE MyAssgn(Arg1, Arg2)
  CLASS(DT0), INTENT(INOUT) :: Arg1
  TYPE(DT0), INTENT(IN)    :: Arg2
    Arg1%ID = -Arg2%ID
  END SUBROUTINE

  END MODULE

  PROGRAM InitExpAssgn3
  USE M
  IMPLICIT NONE

  INTEGER     :: I, J, K

  TYPE :: DT
    TYPE(DT0) :: T
  END TYPE

  TYPE (DT) :: T(128)=[(DT(T=DT0(I)), I=1,128)]

  !IF ( ANY(T%T%ID .NE. [(-I, I=1,128)] )) STOP 11
  IF ( ANY(T%T%ID .NE. [(I, I=1,128)] )) STOP 11

  END



