!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Apr. 14, 2006
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
!*  a reference to an elemental intrinsic
!*
!*  -  SIN
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890




  PROGRAM  InitExpDefElemSIN
  IMPLICIT NONE
  INTEGER :: I, J


  REAL(4), PARAMETER :: r4N_1           = -1._4
  REAL(4), PARAMETER :: r4N_Zero        = REAL(z"80000000", KIND=4)
  REAL(4), PARAMETER :: r4P_Zero        = REAL(z"00000000", KIND=4)
  REAL(4), PARAMETER :: r4P_1           = 1.0_4

  REAL(8), PARAMETER :: r8N_1           = -1._8
  REAL(8), PARAMETER :: r8N_Zero        = REAL(z"8000000000000000", KIND=8)
  REAL(8), PARAMETER :: r8P_Zero        = REAL(z"0000000000000000", KIND=8)
  REAL(8), PARAMETER :: r8P_1           = 1.0_8




  TYPE :: DT4
    REAL(4)  :: R41(4,4)=r4N_1
    REAL(4)  :: R42(4,4)=r4N_Zero
    REAL(4)  :: R43(4,4)=r4P_Zero
    REAL(4)  :: R44(4,4)=r4P_1
  END TYPE

  TYPE :: DT8
    REAL(8)  :: R81(4,4)=r8N_1
    REAL(8)  :: R82(4,4)=r8N_Zero
    REAL(8)  :: R83(4,4)=r8P_Zero
    REAL(8)  :: R84(4,4)=r8P_1
  END TYPE

  TYPE (DT4), PARAMETER :: X4 = DT4()
  TYPE (DT4)            :: T4 = DT4(                       &
                                     R41 = SIN(X=X4%R41),  &
                                     R42 = SIN(X=X4%R42),  &
                                     R43 = SIN(X=X4%R43),  &
                                     R44 = SIN(X=X4%R44)   &
                                   )


  TYPE (DT8), PARAMETER :: X8 = DT8()
  TYPE (DT8)            :: T8 = DT8(                       &
                                     R81 = SIN(X=X8%R81),  &
                                     R82 = SIN(X=X8%R82),  &
                                     R83 = SIN(X=X8%R83),  &
                                     R84 = SIN(x=X8%R84)   &
                                   )


  IF (ANY( ABS(T4%R41+0.8414709568)   .GE. 1.E-6  ))    ERROR STOP 11
  IF (ANY( T4%R42                     .NE. r4P_Zero  )) ERROR STOP 12
  IF (ANY( T4%R43                     .NE. r4N_Zero  )) ERROR STOP 13
  IF (ANY( ABS(T4%R44-0.8414709568)   .GE. 1.E-6  ))    ERROR STOP 14

  IF (ANY( ABS(T8%R81+0.8414709568)   .GE. 1.E-6  ))    ERROR STOP 21
  IF (ANY( T8%R82                     .NE. r8P_Zero  )) ERROR STOP 22
  IF (ANY( T8%R83                     .NE. r8N_Zero  )) ERROR STOP 23
  IF (ANY( ABS(T8%R84-0.8414709568)   .GE. 1.E-6  ))    ERROR STOP 24





  END

