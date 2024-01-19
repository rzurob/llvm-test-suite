!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Aug. 22, 2006
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
!*  ERFC -- An IBM extension
!*
!234567890123456789012345678901234567890123456789012345678901234567890




  PROGRAM  InitExpERFC
  IMPLICIT NONE
  INTEGER :: I, J


  REAL(8), PARAMETER :: R = 0.1572992057_8

  TYPE :: DT
    REAL(4)  :: R4(4,4)=0
    REAL(8)  :: R8(4,4)=0
    REAL(16) :: R6(4,4)=0
  END TYPE


  TYPE (DT)            :: T = DT(                                                      &
                                    R4 = ERFC(X=1.),                                 &
                                    R8 = RESHAPE((/(ERFC(1._8), I=1,16)/), (/4,4/)), &
                                    R6 = ERFC(X=1._16)                               &
                                )


  REAL(KIND(ERFC(X=1.0_4))) :: R4(1) = ERFC(X=1.0_4)
  REAL(KIND(ERFC(X=1.0_8))) :: R8(1) = ERFC(X=1.0_8)
  REAL(KIND(ERFC(X=1.0_16))):: R6(1) = ERFC(X=1.0_16)

  REAL(KIND(DERFC(X=1.0_8))) :: DR(128) = DERFC(X=1.0_8)
  REAL(KIND(QERFC(X=1.0_16))):: QR(128) = QERFC(X=1.0_16)

  IF (ANY( ABS(T%R4-R ) .GT. .1E-5 )) ERROR STOP 11
  IF (ANY( ABS(T%R8-R ) .GT. .1E-5 )) ERROR STOP 12
  IF (ANY( ABS(T%R6-R ) .GT. .1E-5 )) ERROR STOP 13


  IF (KIND(R4) .NE. 4)              ERROR STOP 21
  IF (ANY( ABS(R4-R ) .GT. .1E-5 )) ERROR STOP 22

  IF (KIND(R8) .NE. 8)              ERROR STOP 31
  IF (ANY( ABS(R8-R ) .GT. .1E-5 )) ERROR STOP 32

  IF (KIND(R6) .NE. 16)             ERROR STOP 41
  IF (ANY( ABS(R6-R ) .GT. .1E-5 )) ERROR STOP 42

  IF (KIND(DR) .NE. 8)              ERROR STOP 51
  IF (ANY( ABS(DR-R ) .GT. .1E-5 )) ERROR STOP 52

  IF (KIND(QR) .NE. 16)             ERROR STOP 61
  IF (ANY( ABS(QR-R ) .GT. .1E-5 )) ERROR STOP 62

  END


