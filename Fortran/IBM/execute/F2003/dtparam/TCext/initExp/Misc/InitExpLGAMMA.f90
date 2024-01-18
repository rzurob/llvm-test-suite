! GB DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/initExp/Misc/InitExpLGAMMA.f
! opt variations: -qnol

!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : InitExpLGAMMA.f
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

  TYPE :: DT(N1,K1,K2,K3)    ! (20,4,8,16)
    INTEGER, KIND :: K1,K2,K3
    INTEGER, LEN  :: N1
    REAL(K1)      :: R4(4,4)=0
    REAL(K2)      :: R8(4,4)=0
    REAL(K3)      :: R6(4,4)=0
  END TYPE


  TYPE (DT(20,4,8,16)) :: T = DT(20,4,8,16)(                                  &
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

  IF (ANY( ABS((T%R4-R)/R ) .GT. .1E-5 )) STOP 11
  IF (ANY( ABS((T%R8-R)/R ) .GT. .1E-5 )) STOP 12
  IF (ANY( ABS((T%R6-R)/R ) .GT. .1E-5 )) STOP 13


  IF (KIND(R4) .NE. 4)                 STOP 21
  IF (ANY( ABS((R4-R)/R) .GT. .1E-5 )) STOP 22

  IF (KIND(R8) .NE. 8)                  STOP 31
  IF (ANY( ABS((R8-R)/R ) .GT. .1E-5 )) STOP 32

  IF (KIND(R6) .NE. 16)                 STOP 41
  IF (ANY( ABS((R6-R)/R ) .GT. .1E-5 )) STOP 42

  IF (KIND(AR) .NE. 4)                  STOP 51
  IF (ANY( ABS((AR-R)/R ) .GT. .1E-5 )) STOP 52

  IF (KIND(DR) .NE. 8)                  STOP 61
  IF (ANY( ABS((DR-R)/R ) .GT. .1E-5 )) STOP 62

  IF (KIND(QR) .NE. 16)                 STOP 71
  IF (ANY( ABS((QR-R)/R ) .GT. .1E-5 )) STOP 72


  IF (ANY( R41 .GT. 1)) STOP 22
  IF (ANY( R81 .GT. 1)) STOP 32
  IF (ANY( R61 .GT. 1)) STOP 42
  IF (ANY( AR1 .GT. 1)) STOP 52
  IF (ANY( DR1 .GT. 1)) STOP 62
  IF (ANY( QR1 .GT. 1)) STOP 72

  END


