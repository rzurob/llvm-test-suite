!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : InitExpDefElemSET_EXPONENT.f
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
!*  -  SET_EXPONENT
!*  (318967)
!*
!234567890123456789012345678901234567890123456789012345678901234567890




  PROGRAM  InitExpDefElemSET_EXPONENT
  IMPLICIT NONE
  INTEGER :: I, J


  REAL(4), PARAMETER :: r4Min_N           = REAL(z"80800000", KIND=4)
  REAL(4), PARAMETER :: r4N_Zero          = REAL(z"80000000", KIND=4)
  REAL(4), PARAMETER :: r4P_Zero          = REAL(z"00000000", KIND=4)
  REAL(4), PARAMETER :: r4Min_P           = REAL(z"00800000", KIND=4)


  REAL(8), PARAMETER :: r8Min_N           = REAL(z"8010000000000000", KIND=8)
  REAL(8), PARAMETER :: r8N_Zero          = REAL(z"8000000000000000", KIND=8)
  REAL(8), PARAMETER :: r8P_Zero          = REAL(z"0000000000000000", KIND=8)
  REAL(8), PARAMETER :: r8Min_P           = REAL(z"0010000000000000", KIND=8)



  TYPE :: DT4
    REAL(4)  :: R41(4,4)=r4Min_N
    REAL(4)  :: R42(4,4)=r4N_Zero
    REAL(4)  :: R43(4,4)=r4P_Zero
    REAL(4)  :: R44(4,4)=r4Min_P
    REAL(4)  :: R45(4,4)=4.0
  END TYPE

  TYPE :: DT8
    REAL(8)  :: R81(4,4)=r8Min_N
    REAL(8)  :: R82(4,4)=r8N_Zero
    REAL(8)  :: R83(4,4)=r8P_Zero
    REAL(8)  :: R84(4,4)=r8Min_P
    REAL(8)  :: R85(4,4)=4.0
  END TYPE

  TYPE (DT4), PARAMETER :: X4 = DT4()
  TYPE (DT4)            :: T4 = DT4(                                       &
                                     R41 = SET_EXPONENT(X4%R41, I=-125_1),   &
                                     R42 = SET_EXPONENT(X4%R42, I=16_2),   &
                                     R43 = SET_EXPONENT(X4%R43, I=16_4),   &
                                     R44 = SET_EXPONENT(X4%R44, I=-125_8),   &
                                     R45 = SET_EXPONENT(X4%R45, I=0_8  )    &
                                   )


  TYPE (DT8), PARAMETER :: X8 = DT8()
  TYPE (DT8)            :: T8 = DT8(                                       &
                                     R81 = SET_EXPONENT(X8%R81, I=-1021_2),   &
                                     R82 = SET_EXPONENT(X8%R82, I=16_2),   &
                                     R83 = SET_EXPONENT(X8%R83, I=16_4),   &
                                     R84 = SET_EXPONENT(X8%R84, I=-1021_2),   &
                                     R85 = SET_EXPONENT(X8%R85, I=0_1  )    &
                                   )



  IF (ANY( T4%R41  .NE. r4Min_N  )) STOP 11
  IF (ANY( T4%R42  .NE. 0.0  ))     STOP 12
  IF (ANY( T4%R43  .NE. 0.0  ))     STOP 13
  IF (ANY( T4%R44  .NE. r4Min_P  )) STOP 14
  IF (ANY( T4%R45  .NE. .5  ))      STOP 15


  IF (ANY( T8%R81  .NE. r8Min_N  )) STOP 21
  IF (ANY( T8%R82  .NE. 0.0  ))     STOP 22
  IF (ANY( T8%R83  .NE. 0.0  ))     STOP 23
  IF (ANY( T8%R84  .NE. r8Min_P  )) STOP 24
  IF (ANY( T8%R85  .NE. .5  ))      STOP 25




  END


