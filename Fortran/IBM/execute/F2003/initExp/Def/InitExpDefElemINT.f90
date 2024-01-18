!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : InitExpDefElemINT.f  
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Apr. 10, 2006
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Fortran 2003 Initialization Expression Enhancement 
!*
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  REFERENCE                  : Feature Number 289074 
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  
!*  a reference to an elemental intrinsic
!* 
!*  -  INT 
!*  (319018)
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM  InitExpDefElemINT
  IMPLICIT NONE

  INTEGER :: I, J

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

  

  INTEGER(KIND(INT((/(-1_1, I=1,128)/), KIND=1))), PARAMETER :: I11(128) = INT((/(-1_1, I=1,128)/), KIND=1)
  INTEGER(KIND(INT((/(-1_2, I=1,128)/), KIND=1))), PARAMETER :: I12(128) = INT((/(-1_2, I=1,128)/), KIND=1)
  INTEGER(KIND(INT((/(-1_4, I=1,128)/), KIND=1))), PARAMETER :: I14(128) = INT((/(-1_4, I=1,128)/), KIND=1)
  INTEGER(KIND(INT((/(-1_8, I=1,128)/), KIND=1))), PARAMETER :: I18(128) = INT((/(-1_8, I=1,128)/), KIND=1)

  INTEGER(KIND(INT((/(r4N_DeNormalized, I=1,128)/), KIND=4))), PARAMETER :: I41(128) = INT((/(r4N_DeNormalized, I=1,128)/), KIND=4)
  INTEGER(KIND(INT((/(-0.0,             I=1,128)/), KIND=4))), PARAMETER :: I42(128) = INT((/(-0.0, I=1,128)/), KIND=4)
  INTEGER(KIND(INT((/(+0.0,             I=1,128)/), KIND=4))), PARAMETER :: I43(128) = INT((/(+0.0, I=1,128)/), KIND=4)
  INTEGER(KIND(INT((/(r4P_DeNormalized, I=1,128)/), KIND=4))), PARAMETER :: I44(128) = INT((/(r4P_DeNormalized, I=1,128)/), KIND=4)

  INTEGER(KIND(INT((/(r8N_DeNormalized, I=1,128)/), KIND=8))), PARAMETER ::    &
                          I81(128) = INT((/((r8N_DeNormalized,r8N_Infinity), I=1,128)/), KIND=8)
  INTEGER(KIND(INT((/(-0.0_16,          I=1,128)/), KIND=8))), PARAMETER ::    &
                          I82(128) = INT((/((-0.0_8,r8Signaling_N_NaN), I=1,128)/), KIND=8)
  INTEGER(KIND(INT((/(+0.0_16,          I=1,128)/), KIND=8))), PARAMETER ::    &
                          I83(128) = INT((/((+0.0_8,r8P_DeNormalized), I=1,128)/), KIND=8)
  INTEGER(KIND(INT((/(r8P_DeNormalized, I=1,128)/), KIND=8))), PARAMETER ::    &
                          I84(128) = INT((/((r8P_DeNormalized,r8Quiet_P_NaN), I=1,128)/), KIND=8)


  INTEGER(KIND(INT((/(z"01", I=1,128)/), KIND=1))),     PARAMETER :: IB1 = INT(z"0101", KIND=1)
  INTEGER(KIND(INT((/(z"FF0003", I=1,128)/), KIND=2))), PARAMETER :: IB2 = INT(z"000003", KIND=2)
  INTEGER(KIND(INT((/(z"0001", I=1,128)/), KIND=4))),   PARAMETER :: IB4 = INT(z"0001", KIND=4)
  INTEGER(KIND(INT((/(z"01", I=1,128)/), KIND=8))),     PARAMETER :: IB8 = INT(z"01", KIND=8)

  INTEGER(KIND(INT((/(z"01", I=1,128)/), KIND=1))),     PARAMETER :: IC1(128) = INT((/(z"01", I=1,128)/), KIND=1)
  INTEGER(KIND(INT((/(z"FF0003", I=1,128)/), KIND=2))), PARAMETER :: IC2(128) = INT((/(z"FF0003", I=1,128)/), KIND=2)
  INTEGER(KIND(INT((/(z"0001", I=1,128)/), KIND=4))),   PARAMETER :: IC4(128) = INT((/(z"0001", I=1,128)/), KIND=4)
  INTEGER(KIND(INT((/(z"01", I=1,128)/), KIND=8))),     PARAMETER :: IC8(128) = INT((/(z"01", I=1,128)/), KIND=8)


  IF ( KIND(I11)   .NE.  1 )        STOP 11
  IF ( ANY( I11    .NE. -1 ))       STOP 12
  IF ( KIND(I12)   .NE.  1 )        STOP 13
  IF ( ANY( I12    .NE. -1 ))       STOP 14
  IF ( KIND(I14)   .NE.  1 )        STOP 15
  IF ( ANY( I14    .NE. -1 ))       STOP 16
  IF ( KIND(I18)   .NE.  1 )        STOP 17
  IF ( ANY( I18    .NE. -1 ))       STOP 18

  IF ( KIND(I41)   .NE.  4 )        STOP 21
  IF ( ANY( I41    .NE.  0 ))       STOP 22  
  IF ( KIND(I42)   .NE.  4 )        STOP 23
  IF ( ANY( I42    .NE.  0 ))       STOP 24
  IF ( KIND(I43)   .NE.  4 )        STOP 25
  IF ( ANY( I43    .NE.  0 ))       STOP 26
  IF ( KIND(I44)   .NE.  4 )        STOP 27
  IF ( ANY( I44    .NE.  0 ))       STOP 28

  IF ( KIND(I81)   .NE.  8 )        STOP 31
  IF ( ANY( I81    .NE.  0 ))       STOP 32  
  IF ( KIND(I82)   .NE.  8 )        STOP 33
  IF ( ANY( I82    .NE.  0 ))       STOP 34
  IF ( KIND(I83)   .NE.  8 )        STOP 35
  IF ( ANY( I83    .NE.  0 ))       STOP 36
  IF ( KIND(I84)   .NE.  8 )        STOP 37
  IF ( ANY( I84    .NE.  0 ))       STOP 38

  IF ( KIND(IB1)   .NE.  1 )        STOP 41
  IF (      IB1    .NE.  1 )        STOP 42
  IF ( KIND(IB2)   .NE.  2 )        STOP 43
  IF (      IB2    .NE.  3 )        STOP 44
  IF ( KIND(IB4)   .NE.  4 )        STOP 45
  IF (      IB4    .NE.  1 )        STOP 46
  IF ( KIND(IB8)   .NE.  8 )        STOP 47
  IF (      IB8    .NE.  1 )        STOP 48

  IF ( KIND(IC1)   .NE.  1 )        STOP 51
  IF ( ANY( IC1    .NE.  1 ))       STOP 52
  IF ( KIND(IC2)   .NE.  2 )        STOP 53
  IF ( ANY( IC2    .NE.  3 ))       STOP 54
  IF ( KIND(IC4)   .NE.  4 )        STOP 55
  IF ( ANY( IC4    .NE.  1 ))       STOP 56
  IF ( KIND(IC8)   .NE.  8 )        STOP 57
  IF ( ANY( IC8    .NE.  1 ))       STOP 58

 

  END


