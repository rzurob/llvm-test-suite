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
!*  -  FRACTION
!*  (319003)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  @PROCESS FLOAT(nosingle,hssngl)

  PROGRAM  InitExpDefElemFRACTION
  IMPLICIT NONE
  INTEGER :: I, J, K

  REAL(4), PARAMETER :: r4Quiet_N_NaN     = REAL(z"FFFFFFFF")
  REAL(4), PARAMETER :: r4Signaling_N_NaN = REAL(z"FFBFFFFF")
  REAL(4), PARAMETER :: r4N_Infinity      = REAL(z"FF800000")
  REAL(4), PARAMETER :: r4N_DeNormalized  = REAL(z"80000001")
  REAL(4), PARAMETER :: r4N_Zero          = REAL(z"80000000")
  REAL(4), PARAMETER :: r4P_Zero          = REAL(z"00000000")
  REAL(4), PARAMETER :: r4P_DeNormalized  = REAL(z"00000001")
  REAL(4), PARAMETER :: r4P_Infinity      = REAL(z"7F800000")
  REAL(4), PARAMETER :: r4Signaling_P_NaN = REAL(z"7F800001")
  REAL(4), PARAMETER :: r4Quiet_P_NaN     = REAL(z"7FC00000")

  INTEGER(4), PARAMETER :: i4Quiet_N_NaN     = INT(z"FFFFFFFF")
  INTEGER(4), PARAMETER :: i4Signaling_N_NaN = INT(z"FFBFFFFF")
  INTEGER(4), PARAMETER :: i4N_Infinity      = INT(z"FF800000")
  INTEGER(4), PARAMETER :: i4N_DeNormalized  = INT(z"80000001")
  INTEGER(4), PARAMETER :: i4N_Zero          = INT(z"80000000")
  INTEGER(4), PARAMETER :: i4P_Zero          = INT(z"00000000")
  INTEGER(4), PARAMETER :: i4P_DeNormalized  = INT(z"00000001")
  INTEGER(4), PARAMETER :: i4P_Infinity      = INT(z"7F800000")
  INTEGER(4), PARAMETER :: i4Signaling_P_NaN = INT(z"7F800001")
  INTEGER(4), PARAMETER :: i4Quiet_P_NaN     = INT(z"7FC00000")


  REAL(8), PARAMETER :: r8Quiet_N_NaN     = REAL(z"FFFFFFFFFFFFFFFF", KIND=8)
  REAL(8), PARAMETER :: r8Signaling_N_NaN = REAL(z"FFF7FFFFFFFFFFFF", KIND=8)
  REAL(8), PARAMETER :: r8N_Infinity      = REAL(z"FFF0000000000000", KIND=8)
  REAL(8), PARAMETER :: r8N_DeNormalized  = REAL(z"800FFFFFFFFFFFFF", KIND=8)
  REAL(8), PARAMETER :: r8N_Zero          = REAL(z"8000000000000000", KIND=8)
  REAL(8), PARAMETER :: r8P_Zero          = REAL(z"0000000000000000", KIND=8)
  REAL(8), PARAMETER :: r8P_DeNormalized  = REAL(z"0000000000000001", KIND=8)
  REAL(8), PARAMETER :: r8P_Infinity      = REAL(z"7FF0000000000000", KIND=8)
  REAL(8), PARAMETER :: r8Signaling_P_NaN = REAL(z"7FF0000000000001", KIND=8)
  REAL(8), PARAMETER :: r8Quiet_P_NaN     = REAL(z"7FF8000000000000", KIND=8)


  INTEGER(8), PARAMETER :: i8Quiet_N_NaN     = INT(z"FFFFFFFFFFFFFFFF", KIND=8)
  INTEGER(8), PARAMETER :: i8Signaling_N_NaN = INT(z"FFF7FFFFFFFFFFFF", KIND=8)
  INTEGER(8), PARAMETER :: i8N_Infinity      = INT(z"FFF0000000000000", KIND=8)
  INTEGER(8), PARAMETER :: i8N_DeNormalized  = INT(z"800FFFFFFFFFFFFF", KIND=8)
  INTEGER(4), PARAMETER :: i8N_Zero          = INT(z"8000000000000000", KIND=8)
  INTEGER(4), PARAMETER :: i8P_Zero          = INT(z"0000000000000000", KIND=8)
  INTEGER(8), PARAMETER :: i8P_DeNormalized  = INT(z"0000000000000001", KIND=8)
  INTEGER(8), PARAMETER :: i8P_Infinity      = INT(z"7FF0000000000000", KIND=8)
  INTEGER(8), PARAMETER :: i8Signaling_P_NaN = INT(z"7FF0000000000001", KIND=8)
  INTEGER(8), PARAMETER :: i8Quiet_P_NaN     = INT(z"7FF8000000000000", KIND=8)




  REAL(KIND(FRACTION((/(r4Quiet_N_NaN, I=1,8)/)))) ::            &
       R40(8)= FRACTION((/(r4Quiet_N_NaN, I=1,8)/))
  REAL(KIND(FRACTION((/(r4Signaling_N_NaN, I=1,8)/)))) ::        &
       R41(8)= FRACTION((/(r4Signaling_N_NaN, I=1,8)/))
  REAL(KIND(FRACTION((/(r4N_Infinity, I=1,8)/)))) ::             &
       R42(8)= FRACTION((/(r4N_Infinity, I=1,8)/))
  REAL(KIND(FRACTION((/(r4N_DeNormalized, I=1,8)/)))) ::         &
       R43(8)= FRACTION((/(r4N_DeNormalized, I=1,8)/))
  REAL(KIND(FRACTION((/(r4N_Zero, I=1,8)/)))) ::                 &
       R44(8)= FRACTION((/(r4N_Zero, I=1,8)/))
  REAL(KIND(FRACTION((/(r4P_Zero, I=1,8)/)))) ::                 &
       R45(8)= FRACTION((/(r4P_Zero, I=1,8)/))
  REAL(KIND(FRACTION((/(r4P_DeNormalized, I=1,8)/)))) ::         &
       R46(8)= FRACTION((/(r4P_DeNormalized, I=1,8)/))
  REAL(KIND(FRACTION((/(r4P_Infinity, I=1,8)/)))) ::             &
       R47(8)= FRACTION((/(r4P_Infinity, I=1,8)/))
  REAL(KIND(FRACTION((/(r4Signaling_P_NaN, I=1,8)/)))) ::        &
       R48(8)= FRACTION((/(r4Signaling_P_NaN, I=1,8)/))
  REAL(KIND(FRACTION((/(r4Quiet_P_NaN, I=1,8)/)))) ::            &
       R49(8)= FRACTION((/(r4Quiet_P_NaN, I=1,8)/))


  REAL(KIND(FRACTION((/(r8Quiet_N_NaN, I=1,8)/)))) ::            &
       R80(8)= FRACTION((/(r8Quiet_N_NaN, I=1,8)/))
  REAL(KIND(FRACTION((/(r8Signaling_N_NaN, I=1,8)/)))) ::        &
       R81(8)= FRACTION((/(r8Signaling_N_NaN, I=1,8)/))
  REAL(KIND(FRACTION((/(r8N_Infinity, I=1,8)/)))) ::             &
       R82(8)= FRACTION((/(r8N_Infinity, I=1,8)/))
  REAL(KIND(FRACTION((/(r8N_DeNormalized, I=1,8)/)))) ::         &
       R83(8)= FRACTION((/(r8N_DeNormalized, I=1,8)/))
  REAL(KIND(FRACTION((/(r8N_Zero, I=1,8)/)))) ::                 &
       R84(8)= FRACTION((/(r8N_Zero, I=1,8)/))
  REAL(KIND(FRACTION((/(r8P_Zero, I=1,8)/)))) ::                 &
       R85(8)= FRACTION((/(r8P_Zero, I=1,8)/))
  REAL(KIND(FRACTION((/(r8P_DeNormalized, I=1,8)/)))) ::         &
       R86(8)= FRACTION((/(r8P_DeNormalized, I=1,8)/))
  REAL(KIND(FRACTION((/(r8P_Infinity, I=1,8)/)))) ::             &
       R87(8)= FRACTION((/(r8P_Infinity, I=1,8)/))
  REAL(KIND(FRACTION((/(r8Signaling_P_NaN, I=1,8)/)))) ::        &
       R88(8)= FRACTION((/(r8Signaling_P_NaN, I=1,8)/))
  REAL(KIND(FRACTION((/(r8Quiet_P_NaN, I=1,8)/)))) ::            &
       R89(8)= FRACTION((/(r8Quiet_P_NaN, I=1,8)/))

  IF ( KIND(R40)                                .NE. 4              )      ERROR STOP 11
  IF ( ANY(TRANSFER(R40, i4Quiet_N_NaN, 8)      .NE. i4Quiet_N_NaN  ) )    ERROR STOP 12

  IF ( KIND(R41)                                .NE. 4                  )  ERROR STOP 13
  IF ( ANY(TRANSFER(R41, i4Signaling_N_NaN, 8)  .NE. i4Signaling_N_NaN  )) ERROR STOP 14

  IF ( KIND(R42)                                .NE. 4             )       ERROR STOP 15
  IF ( ANY( TRANSFER(R42, i4N_Infinity, 8)      .NE. i4N_Infinity  ) )     ERROR STOP 16


  IF ( KIND(R43)     .NE. 4                           )      ERROR STOP 17
  IF ( ANY( R43      .NE. FRACTION(r4N_DeNormalized)  ) )    ERROR STOP 18

  IF ( KIND(R44)     .NE. 4                           )      ERROR STOP 19
  IF ( ANY( R44      .NE. FRACTION(r4N_Zero)          ) )    ERROR STOP 20

  IF ( KIND(R45)     .NE. 4                           )      ERROR STOP 21
  IF ( ANY( R45      .NE. FRACTION(r4P_Zero)          ) )    ERROR STOP 22

  IF ( KIND(R46)     .NE. 4                           )      ERROR STOP 23
  IF ( ANY( R46      .NE. FRACTION(r4p_DeNormalized)  ) )    ERROR STOP 24

  IF ( KIND(R47)                              .NE. 4             )        ERROR STOP 25
  IF ( ANY( TRANSFER(R47, i4P_Infinity, 8)    .NE. i4P_Infinity  ) )      ERROR STOP 26

  IF ( KIND(R48)                              .NE. 4                  )   ERROR STOP 27
  IF ( ANY(TRANSFER(R48, i4Signaling_P_NaN, 8).NE. i4Signaling_P_NaN  ) ) ERROR STOP 28

  IF ( KIND(R49)                              .NE. 4              )       ERROR STOP 29
  IF ( ANY( TRANSFER(R49, i4Quiet_P_NaN, 8)   .NE. i4Quiet_P_NaN  ) )     ERROR STOP 30



  IF ( KIND(R80)                                .NE. 8              )      ERROR STOP 31
  IF ( ANY(TRANSFER(R80, i8Quiet_N_NaN, 8)      .NE. i8Quiet_N_NaN  ) )    ERROR STOP 32

  IF ( KIND(R81)                                .NE. 8                  )  ERROR STOP 33
  IF ( ANY(TRANSFER(R81, i8Signaling_N_NaN, 8)  .NE. i8Signaling_N_NaN  )) ERROR STOP 34

  IF ( KIND(R82)                                .NE. 8             )       ERROR STOP 35
  IF ( ANY( TRANSFER(R82, i8N_Infinity, 8)      .NE. i8N_Infinity  ) )     ERROR STOP 36


  IF ( KIND(R83)     .NE. 8                           )      ERROR STOP 37
  IF ( ANY( R83      .NE. FRACTION(r8N_DeNormalized)  ) )    ERROR STOP 38
  IF ( KIND(R84)     .NE. 8                           )      ERROR STOP 39
  IF ( ANY( R84      .NE. FRACTION(r8N_Zero)          ) )    ERROR STOP 40

  IF ( KIND(R85)     .NE. 8                           )      ERROR STOP 41
  IF ( ANY( R85      .NE. FRACTION(r8P_Zero)          ) )    ERROR STOP 42

  IF ( KIND(R86)     .NE. 8                           )      ERROR STOP 43
  IF ( ANY( R86      .NE. FRACTION(r8p_DeNormalized)  ) )    ERROR STOP 44

  IF ( KIND(R87)                              .NE. 8             )        ERROR STOP 45
  IF ( ANY( TRANSFER(R87, i8P_Infinity, 8)    .NE. i8P_Infinity  ) )      ERROR STOP 46

  IF ( KIND(R88)                              .NE. 8                  )   ERROR STOP 47
  IF ( ANY(TRANSFER(R88, i8Signaling_P_NaN, 8).NE. i8Signaling_P_NaN  ) ) ERROR STOP 48

  IF ( KIND(R89)                              .NE. 8              )       ERROR STOP 49
  IF ( ANY( TRANSFER(R89, i8Quiet_P_NaN, 8)   .NE. i8Quiet_P_NaN  ) )     ERROR STOP 50


  END


