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
!*  LGAMMA -- An IBM extension
!* (324496)
!*
!234567890123456789012345678901234567890123456789012345678901234567890




  PROGRAM  InitExpLGAMMA
  IMPLICIT NONE
  INTEGER :: I, J


  REAL(8), PARAMETER :: R = 12.80182743_8

  TYPE :: DT
    REAL(4)  :: R4(4,4)=0
    REAL(8)  :: R8(4,4)=0
    REAL(16) :: R6(4,4)=0
  END TYPE


  TYPE (DT)            :: T = DT(                                                       &
                                    R4 = LGAMMA(X=10.),                                 &
                                    R8 = RESHAPE((/(LGAMMA(10._8), I=1,16)/), (/4,4/)), &
                                    R6 = LGAMMA(X=10._16)                               &
                                )


  REAL(KIND(LGAMMA(X=10.0_4))) :: R4(128) = LGAMMA(X=10.0_4)
  REAL(KIND(LGAMMA(X=10.0_8))) :: R8(128) = LGAMMA(X=10.0_8)
  REAL(KIND(LGAMMA(X=10.0_16))):: R6(128) = LGAMMA(X=10.0_16)

  REAL(KIND(ALGAMA(X=10.0_4))) :: AR(128) = ALGAMA(X=10.0_4)
  REAL(KIND(DLGAMA(X=10.0_8))) :: DR(128) = DLGAMA(X=10.0_8)
  REAL(KIND(QLGAMA(X=10.0_16))):: QR(128) = QLGAMA(X=10.0_16)

  REAL(KIND(LGAMMA(X=1.0_4))) :: R41(128) = LGAMMA(X=1.0_4)
  REAL(KIND(LGAMMA(X=1.0_8))) :: R81(128) = LGAMMA(X=1.0_8)
  REAL(KIND(LGAMMA(X=1.0_16))):: R61(128) = LGAMMA(X=1.0_16)

  REAL(KIND(ALGAMA(X=1.0_4))) :: AR1(128) = ALGAMA(X=1.0_4)
  REAL(KIND(DLGAMA(X=1.0_8))) :: DR1(128) = DLGAMA(X=1.0_8)
  REAL(KIND(QLGAMA(X=1.0_16))):: QR1(128) = QLGAMA(X=1.0_16)

  IF (ANY( ABS((T%R4-R)/R ) .GT. .1E-5 )) ERROR STOP 11
  IF (ANY( ABS((T%R8-R)/R ) .GT. .1E-5 )) ERROR STOP 12
  IF (ANY( ABS((T%R6-R)/R ) .GT. .1E-5 )) ERROR STOP 13


  IF (KIND(R4) .NE. 4)                 ERROR STOP 21
  IF (ANY( ABS((R4-R)/R) .GT. .1E-5 )) ERROR STOP 22

  IF (KIND(R8) .NE. 8)                  ERROR STOP 31
  IF (ANY( ABS((R8-R)/R ) .GT. .1E-5 )) ERROR STOP 32

  IF (KIND(R6) .NE. 16)                 ERROR STOP 41
  IF (ANY( ABS((R6-R)/R ) .GT. .1E-5 )) ERROR STOP 42

  IF (KIND(AR) .NE. 4)                  ERROR STOP 51
  IF (ANY( ABS((AR-R)/R ) .GT. .1E-5 )) ERROR STOP 52

  IF (KIND(DR) .NE. 8)                  ERROR STOP 61
  IF (ANY( ABS((DR-R)/R ) .GT. .1E-5 )) ERROR STOP 62

  IF (KIND(QR) .NE. 16)                 ERROR STOP 71
  IF (ANY( ABS((QR-R)/R ) .GT. .1E-5 )) ERROR STOP 72


  IF (ANY( R41 .GT. 1)) ERROR STOP 22
  IF (ANY( R81 .GT. 1)) ERROR STOP 32
  IF (ANY( R61 .GT. 1)) ERROR STOP 42
  IF (ANY( AR1 .GT. 1)) ERROR STOP 52
  IF (ANY( DR1 .GT. 1)) ERROR STOP 62
  IF (ANY( QR1 .GT. 1)) ERROR STOP 72

  END

