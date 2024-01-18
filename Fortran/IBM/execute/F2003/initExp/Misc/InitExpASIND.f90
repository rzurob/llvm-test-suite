!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Aug. 16, 2006
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
!*  ASIND - an IBM extension
!*
!*  (324206)
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM InitExpASIND
  IMPLICIT NONE
  INTEGER :: I, J, K

  REAL(4), PARAMETER        :: X4(30)=0.5
  REAL(8), PARAMETER        :: X8(30)=0.5_8
  REAL(16),PARAMETER        :: X6(30)=0.5_16

  REAL(KIND(ASIND(X4))), PARAMETER :: R4(SIZE(X4)) = ASIND(X4)
  REAL(KIND(ASIND(X8))), PARAMETER :: R8(SIZE(X8)) = ASIND(X8)
  REAL(KIND(ASIND(X6))), PARAMETER :: R6(SIZE(X6)) = ASIND(X6)

  COMPLEX(KIND(ASIND(R4))) :: Z4(SIZE(R4)) = (/((ASIND(X4(I)), ASIND(X4(I))), I=1, SIZE(R4))/)
  COMPLEX(KIND(ASIND(R8))) :: Z8(SIZE(R8)) = (/((ASIND(X8(I)), ASIND(X8(I))), I=1, SIZE(R8))/)
  COMPLEX(KIND(ASIND(R6))) :: Z6(SIZE(R6)) = (/((ASIND(X6(I)), ASIND(X6(I))), I=1, SIZE(R6))/)



  IF (KIND(R4)  .NE. 4   )                                 STOP 11
  IF (SIZE(R4)  .NE. 30   )                                STOP 12
  IF (ANY(R4    .NE. 30.0) )                               STOP 13

  IF (KIND(R8)  .NE. 8   )                                 STOP 21
  IF (SIZE(R8)  .NE. 30   )                                STOP 22
  IF (ANY(ABS(R8-30.0)  .GT. 1.E-6) )                      STOP 23

  IF (KIND(R6)  .NE. 16  )                                 STOP 31
  IF (SIZE(R6)  .NE. 30   )                                STOP 32
  IF (ANY(R6    .NE. 30.0) )                               STOP 33


  IF (KIND(Z4)  .NE. 4   )                                 STOP 41
  IF (SIZE(Z4)  .NE. 30   )                                STOP 42
  IF (ANY(ABS(Z4-(30.0,30.0))  .GT. 1.E-6) )               STOP 43

  IF (KIND(Z8)  .NE. 8   )                                 STOP 51
  IF (SIZE(Z8)  .NE. 30   )                                STOP 52
  IF (ANY(ABS(Z8-(30.0,30.0))  .GT. 1.E-6) )               STOP 53

  IF (KIND(Z6)  .NE. 16  )                                 STOP 61
  IF (SIZE(Z6)  .NE. 30   )                                STOP 62
  IF (ANY(ABS(Z6-(30.0,30.0))  .GT. 1.E-6) )               STOP 63


  END


