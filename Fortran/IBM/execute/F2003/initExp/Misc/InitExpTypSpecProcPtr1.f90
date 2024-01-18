!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : InitExpTypSpecProcPtr1.f
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
!*  intrinsic-type-spec in proc ptr: real and complex
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  CONTAINS

  FUNCTION R4()
    REAL(4) :: R4
    R4 = 4
  END FUNCTION

  FUNCTION R8()
    REAL(8) :: R8
    R8 = 8
  END FUNCTION

  FUNCTION R6()
    REAL(16) :: R6
    R6 = 16.
  END FUNCTION


  FUNCTION Z4()
    COMPLEX(4) :: Z4
    Z4 = (4., -4.)
  END FUNCTION

  FUNCTION Z8()
    COMPLEX(8) :: Z8
    Z8 = (8., -8.)
  END FUNCTION

  FUNCTION Z6()
    COMPLEX(16) :: Z6
    Z6 = (16., -16.)
  END FUNCTION


  END MODULE


  PROGRAM InitExpTypSpecProcPtr1
  USE M
  IMPLICIT NONE

  INTEGER :: I


  PROCEDURE(REAL(KIND=4_8)),                 POINTER :: R4ProcPtr
  PROCEDURE(REAL(KIND=SIZE([(i,i=1,8)]))),   POINTER :: R8ProcPtr
  PROCEDURE(REAL(KIND=SIZE([(i,i=1,16)]))),  POINTER :: R6ProcPtr

  PROCEDURE(COMPLEX(KIND=4_1)),                 POINTER :: Z4ProcPtr
  PROCEDURE(COMPLEX(KIND=SIZE([(i,i=1,8)]))),   POINTER :: Z8ProcPtr
  PROCEDURE(COMPLEX(KIND=SIZE([(i,i=1,16)]))),  POINTER :: Z6ProcPtr


  R4ProcPtr => R4
  R8ProcPtr => R8
  R6ProcPtr => R6

  Z4ProcPtr => Z4
  Z8ProcPtr => Z8
  Z6ProcPtr => Z6


  IF ( R4ProcPtr() .NE. 4.  ) STOP 11
  IF ( R8ProcPtr() .NE. 8.  ) STOP 12
  IF ( R6ProcPtr() .NE. 16. ) STOP 13

  IF ( Z4ProcPtr() .NE. (4., -4.))  STOP 21
  IF ( Z8ProcPtr() .NE. (8., -8.))  STOP 22
  IF ( Z6ProcPtr() .NE. (16.,-16.)) STOP 23

  END


