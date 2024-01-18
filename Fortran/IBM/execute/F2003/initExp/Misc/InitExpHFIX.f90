!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Aug. 24, 2006
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
!* HFIX -- An IBM extension
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM  InitExpHFIX
  IMPLICIT NONE

  INTEGER :: I, J

  REAL(4), PARAMETER :: r4N_DeNormalized  = REAL(z"80000001", KIND=4)
  REAL(4), PARAMETER :: r4N_Zero          = REAL(z"80000000", KIND=4)
  REAL(4), PARAMETER :: r4P_Zero          = REAL(z"00000000", KIND=4)
  REAL(4), PARAMETER :: r4P_DeNormalized  = REAL(z"00000001", KIND=4)


  INTEGER(KIND(HFIX((/(r4N_DeNormalized, I=1,128)/)))), PARAMETER :: I21(128) = HFIX((/(r4N_DeNormalized, I=1,128)/))
  INTEGER(KIND(HFIX((/(r4N_Zero,         I=1,128)/)))), PARAMETER :: I22(128) = HFIX((/(r4N_DeNormalized, I=1,128)/))
  INTEGER(KIND(HFIX((/(r4P_Zero,         I=1,128)/)))), PARAMETER :: I23(128) = HFIX((/(r4N_DeNormalized, I=1,128)/))
  INTEGER(KIND(HFIX((/(r4N_DeNormalized, I=1,128)/)))), PARAMETER :: I24(128) = HFIX((/(r4N_DeNormalized, I=1,128)/))
  INTEGER(KIND(HFIX((/(-3.7, I=1,128)/)))),             PARAMETER :: I25(128) = HFIX((/(-3.7, I=1,128)/))
  INTEGER(KIND(HFIX((/(-32768.99, I=1,128)/)))),        PARAMETER :: I26(128) = HFIX((/(-32768.99, I=1,128)/))
  INTEGER(KIND(HFIX((/(32767.99, I=1,128)/)))),         PARAMETER :: I27(128) = HFIX((/(32767.99, I=1,128)/))


  IF ( KIND(I21)   .NE.  2 )        STOP 11
  IF ( ANY( I21    .NE.  0 ))       STOP 12

  IF ( KIND(I22)   .NE.  2 )        STOP 21
  IF ( ANY( I22    .NE.  0 ))       STOP 22

  IF ( KIND(I23)   .NE.  2 )        STOP 31
  IF ( ANY( I23    .NE.  0 ))       STOP 32

  IF ( KIND(I23)   .NE.  2 )        STOP 41
  IF ( ANY( I23    .NE.  0 ))       STOP 42

  IF ( KIND(I25)   .NE.  2 )        STOP 51
  IF ( ANY( I25    .NE. -3 ))       STOP 52

  IF ( KIND(I26)   .NE.  2 )        STOP 61
  IF ( ANY( I26    .NE. -32768 ))   STOP 62

  IF ( KIND(I27)   .NE.  2 )        STOP 71
  IF ( ANY( I27    .NE.  32767 ))   STOP 72


  END


