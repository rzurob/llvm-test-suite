!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Apr. 07, 2006
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
!*  -  FLOOR
!*  (318996)
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM  InitExpDefElemFLOOR
  IMPLICIT NONE
  INTEGER :: I, J, K

  REAL(4), PARAMETER :: r4Quiet_N_NaN     = REAL(z"FFFFFFFF", KIND=4)
  REAL(4), PARAMETER :: r4Signaling_N_NaN = REAL(z"FFBFFFFF", KIND=4)
  REAL(4), PARAMETER :: r4N_Infinity      = REAL(z"FF800000", KIND=4)
  REAL(4), PARAMETER :: r4N_DeNormalized  = REAL(z"80000001", KIND=4)
  REAL(4), PARAMETER :: r4P_DeNormalized  = REAL(z"00000001", KIND=4)
  REAL(4), PARAMETER :: r4P_Infinity      = REAL(z"7F800000", KIND=4)
  REAL(4), PARAMETER :: r4Signaling_P_NaN = REAL(z"7F800001", KIND=4)
  REAL(4), PARAMETER :: r4Quiet_P_NaN     = REAL(z"7FC00000", KIND=4)

  REAL(8), PARAMETER :: r8Quiet_N_NaN     = REAL(z"FFFFFFFFFFFFFFFF", KIND=8)
  REAL(8), PARAMETER :: r8Signaling_N_NaN = REAL(z"FFF7FFFFFFFFFFFF", KIND=8)
  REAL(8), PARAMETER :: r8N_Infinity      = REAL(z"FFF0000000000000", KIND=8)
  REAL(8), PARAMETER :: r8N_DeNormalized  = REAL(z"800FFFFFFFFFFFFF", KIND=8)
  REAL(8), PARAMETER :: r8P_DeNormalized  = REAL(z"0000000000000001", KIND=8)
  REAL(8), PARAMETER :: r8P_Infinity      = REAL(z"7FF0000000000000", KIND=8)
  REAL(8), PARAMETER :: r8Signaling_P_NaN = REAL(z"7FF0000000000001", KIND=8)
  REAL(8), PARAMETER :: r8Quiet_P_NaN     = REAL(z"7FF8000000000000", KIND=8)

  INTEGER(4), PARAMETER :: I4PMax=HUGE(0_4)
  INTEGER(4), PARAMETER :: I4NMax=-I4PMAX-1
  INTEGER(8), PARAMETER :: I8PMax=HUGE(0._8)
  INTEGER(8), PARAMETER :: I8NMax=-I8PMAX-1

  INTEGER(KIND(FLOOR((/(r4Quiet_N_NaN, I=1,8)/), KIND=4))) ::            &
       I41(8)= FLOOR((/(r4Quiet_N_NaN, I=1,8)/), KIND=4)
  INTEGER(KIND(FLOOR((/(r4Signaling_N_NaN, I=1,8)/), KIND=4))) ::        &
       I42(8)= FLOOR((/(r4Signaling_N_NaN, I=1,8)/), KIND=4)
  INTEGER(KIND(FLOOR((/(r4N_Infinity, I=1,8)/), KIND=4))) ::             &
       I43(8)= FLOOR((/(r4N_Infinity, I=1,8)/), KIND=4)
  INTEGER(KIND(FLOOR((/(r4N_DeNormalized, I=1,8)/), KIND=4))) ::         &
       I44(8)= FLOOR((/(r4N_DeNormalized, I=1,8)/), KIND=4)
  INTEGER(KIND(FLOOR((/(r4P_DeNormalized, I=1,8)/), KIND=4))) ::         &
       I45(8)= FLOOR((/(r4P_DeNormalized, I=1,8)/), KIND=4)
  INTEGER(KIND(FLOOR((/(r4P_Infinity, I=1,8)/), KIND=4))) ::             &
       I46(8)= FLOOR((/(r4P_Infinity, I=1,8)/), KIND=4)
  INTEGER(KIND(FLOOR((/(r4Signaling_P_NaN, I=1,8)/), KIND=4))) ::        &
       I47(8)= FLOOR((/(r4Signaling_P_NaN, I=1,8)/), KIND=4)
  INTEGER(KIND(FLOOR((/(r4Quiet_P_NaN, I=1,8)/), KIND=4))) ::            &
       I48(8)= FLOOR((/(r4Quiet_P_NaN, I=1,8)/), KIND=4)


  INTEGER(KIND(FLOOR((/(r8Quiet_N_NaN, I=1,8)/), KIND=8))) ::            &
       I81(8)= FLOOR((/(r8Quiet_N_NaN, I=1,8)/), KIND=8)
  INTEGER(KIND(FLOOR((/(r8Signaling_N_NaN, I=1,8)/), KIND=8))) ::        &
       I82(8)= FLOOR((/(r8Signaling_N_NaN, I=1,8)/), KIND=8)
  INTEGER(KIND(FLOOR((/(r8N_Infinity, I=1,8)/), KIND=8))) ::             &
       I83(8)= FLOOR((/(r8N_Infinity, I=1,8)/), KIND=8)
  INTEGER(KIND(FLOOR((/(r8N_DeNormalized, I=1,8)/), KIND=8))) ::         &
       I84(8)= FLOOR((/(r8N_DeNormalized, I=1,8)/), KIND=8)
  INTEGER(KIND(FLOOR((/(r8P_DeNormalized, I=1,8)/), KIND=8))) ::         &
       I85(8)= FLOOR((/(r8P_DeNormalized, I=1,8)/), KIND=8)
  INTEGER(KIND(FLOOR((/(r8P_Infinity, I=1,8)/), KIND=8))) ::             &
       I86(8)= FLOOR((/(r8P_Infinity, I=1,8)/), KIND=8)
  INTEGER(KIND(FLOOR((/(r8Signaling_P_NaN, I=1,8)/), KIND=8))) ::        &
       I87(8)= FLOOR((/(r8Signaling_P_NaN, I=1,8)/), KIND=8)
  INTEGER(KIND(FLOOR((/(r8Quiet_P_NaN, I=1,8)/), KIND=8))) ::            &
       I88(8)= FLOOR((/(r8Quiet_P_NaN, I=1,8)/), KIND=8)


! print*,floor(r4Quiet_N_NaN)
! print*,floor(r4Signaling_N_NaN)
! print*,floor(r4N_Infinity)
! print*,floor(r4N_DeNormalized)
! print*,floor(r4p_DeNormalized)
! print*,floor(r4P_Infinity)
! print*,floor(r4Signaling_P_NaN)
! print*,floor(r4Quiet_P_NaN)
! print*, "============="
! print*,floor(r8Quiet_N_NaN)
! print*,floor(r8Signaling_N_NaN)
! print*,floor(r8N_Infinity)
! print*,floor(r8N_DeNormalized, kind=8)
! print*,floor(r8p_DeNormalized)
! print*,floor(r8P_Infinity)
! print*,floor(r8Signaling_P_NaN)
! print*,floor(r8Quiet_P_NaN)



  IF ( KIND(I41)  .NE. 4      )      ERROR STOP 11
  IF ( ANY( I41   .NE. I4NMAX  ) )   ERROR STOP 12

  IF ( KIND(I42)  .NE. 4      )      ERROR STOP 13
  IF ( ANY( I42   .NE. I4NMAX  ) )   ERROR STOP 14

  IF ( KIND(I43)  .NE. 4      )      ERROR STOP 15
  IF ( ANY( I43   .NE. I4NMAX  ) )   ERROR STOP 16

  IF ( KIND(I44)  .NE. 4   )         ERROR STOP 17
  IF ( ANY( I44   .NE. -1  ) )       ERROR STOP 18

  IF ( KIND(I45)  .NE. 4  )          ERROR STOP 19
  IF ( ANY( I45   .NE. 0  ) )        ERROR STOP 20

  IF ( KIND(I46)  .NE. 4      )      ERROR STOP 21
  IF ( ANY( I46   .NE. I4PMAX  ) )   ERROR STOP 22

  IF ( KIND(I47)  .NE. 4      )      ERROR STOP 23
! IF ( ANY( I47   .NE. I4PMAX  ) )   ERROR STOP 24

  IF ( KIND(I48)  .NE. 4      )      ERROR STOP 25
! IF ( ANY( I48   .NE. I4PMAX  ) )   ERROR STOP 26


  IF ( KIND(I81)  .NE. 8      )      ERROR STOP 31
  IF ( ANY( I81   .NE. I8NMAX  ) )   ERROR STOP 32

  IF ( KIND(I82)  .NE. 8      )      ERROR STOP 33
  IF ( ANY( I82   .NE. I8NMAX  ) )   ERROR STOP 34

  IF ( KIND(I83)  .NE. 8      )      ERROR STOP 35
  IF ( ANY( I83   .NE. I8NMAX  ) )   ERROR STOP 36

  IF ( KIND(I84)  .NE. 8   )         ERROR STOP 37
  IF ( ANY( I84   .NE. -1  ) )       ERROR STOP 38

  IF ( KIND(I85)  .NE. 8  )          ERROR STOP 39
  IF ( ANY( I85   .NE. 0  ) )        ERROR STOP 40

  IF ( KIND(I86)  .NE. 8      )      ERROR STOP 41
  IF ( ANY( I86   .NE. I8PMAX  ) )   ERROR STOP 42

  IF ( KIND(I87)  .NE. 8      )      ERROR STOP 43
! IF ( ANY( I87   .NE. I8PMAX  ) )   ERROR STOP 44

  IF ( KIND(I88)  .NE. 8      )      ERROR STOP 45
! IF ( ANY( I88   .NE. I8PMAX  ) )   ERROR STOP 46

  END


