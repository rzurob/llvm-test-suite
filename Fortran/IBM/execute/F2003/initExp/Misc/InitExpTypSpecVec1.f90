!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : InitExpTypSpecVec1.f
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
!*  vector subscript
!*
!234567890123456789012345678901234567890123456789012345678901234567890




  PROGRAM InitExpTypSpecVec1
  IMPLICIT NONE

  INTEGER :: I

  REAL(4),  PARAMETER :: RV4(-128:-1)=[(I, I=-128, -1)]
  REAL(8),  PARAMETER :: RV8(-128:-1)=[(I, I=-128, -1)]
  REAL(16), PARAMETER :: RV6(-128:-1)=[(I, I=-128, -1)]

  COMPLEX(4),  PARAMETER :: ZV4(-128:-1)=[((I,-I), I=-128, -1)]
  COMPLEX(8),  PARAMETER :: ZV8(-128:-1)=[((I,-I), I=-128, -1)]
  COMPLEX(16), PARAMETER :: ZV6(-128:-1)=[((I,-I), I=-128, -1)]

  INTEGER(1),  PARAMETER :: S1(-128:-1)=[(I, I=-1,-128, -1)]
  INTEGER(2),  PARAMETER :: S2(-128:-1)=[(I, I=-1,-128, -1)]
  INTEGER(4),  PARAMETER :: S4(-128:-1)=[(I, I=-1,-128, -1)]
  INTEGER(8),  PARAMETER :: S8(-128:-1)=[(I, I=-1,-128, -1)]

  REAL,     PARAMETER :: RR(128)=[(I,      I=-1, -128, -1)]
  COMPLEX,  PARAMETER :: ZR(128)=[((I,-I), I=-1, -128, -1)]


  REAL(KIND=4),  PARAMETER :: R4(128)=RV4(S1)
  REAL(KIND=8),  PARAMETER :: R8(128)=RV8(S2)
  REAL(KIND=16), PARAMETER :: R6(128)=RV6(S8)

  COMPLEX(KIND=4),  PARAMETER :: Z4(128)=ZV4(S1)
  COMPLEX(KIND=8),  PARAMETER :: Z8(128)=ZV8(S2)
  COMPLEX(KIND=16), PARAMETER :: Z6(128)=ZV6(S8)



  IF ( ANY(R4   .NE. RR ) ) STOP 22
  IF ( ANY(R8   .NE. RR ) ) STOP 23
  IF ( ANY(R6   .NE. RR ) ) STOP 24


  IF ( ANY(Z4   .NE. ZR ) ) STOP 42
  IF ( ANY(Z8   .NE. ZR ) ) STOP 43
  IF ( ANY(Z6   .NE. ZR ) ) STOP 44

  END


