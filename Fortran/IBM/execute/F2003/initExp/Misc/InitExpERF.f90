!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : InitExpERF.f
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
!*  ERF -- An IBM extension
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM  InitExpERF
  IMPLICIT NONE
  INTEGER :: I, J


  REAL(8), PARAMETER :: R = 0.8427007794_8

  TYPE :: DT
    REAL(4)  :: R4(4,4)=0
    REAL(8)  :: R8(4,4)=0
    REAL(16) :: R6(4,4)=0
  END TYPE


  TYPE (DT)            :: T = DT(                                                      &
                                    R4 = ERF(X=1.),                                 &
                                    R8 = RESHAPE((/(ERF(1._8), I=1,16)/), (/4,4/)), &
                                    R6 = ERF(X=1._16)                               &
                                )


  REAL(KIND(ERF(X=1.0_4))) :: R4(1) = ERF(X=1.0_4)
  REAL(KIND(ERF(X=1.0_8))) :: R8(1) = ERF(X=1.0_8)
  REAL(KIND(ERF(X=1.0_16))):: R6(1) = ERF(X=1.0_16)


  IF (ANY( ABS(T%R4-R ) .GT. .1E-5 )) STOP 11
  IF (ANY( ABS(T%R8-R ) .GT. .1E-5 )) STOP 12
  IF (ANY( ABS(T%R6-R ) .GT. .1E-5 )) STOP 13

  IF (KIND(R4) .NE. 4)              STOP 21
  IF (ANY( ABS(R4-R ) .GT. .1E-5 )) STOP 22

  IF (KIND(R8) .NE. 8)              STOP 31
  IF (ANY( ABS(R8-R ) .GT. .1E-5 )) STOP 32

  IF (KIND(R6) .NE. 16)             STOP 41
  IF (ANY( ABS(R6-R ) .GT. .1E-5 )) STOP 42

  END


