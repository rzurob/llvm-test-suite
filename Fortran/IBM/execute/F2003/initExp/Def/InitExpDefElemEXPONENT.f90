!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : InitExpDefElemEXPONENT.f
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
!*  -  EXPONENT
!*  (318985/319550)
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM  InitExpDefElemEXPONENT
  USE IEEE_ARITHMETIC
  IMPLICIT NONE
  INTEGER :: I, J, K


  TYPE :: DT0
    REAL(4)      :: XR4(4) =  4.2
    REAL(8)      :: XR8(4) =  4.2
    REAL(16)     :: XR6(4) =  4.2
  END TYPE

  TYPE :: DT
    INTEGER :: E1(4)
    INTEGER :: E2(4)
    INTEGER :: E3(4)
  END TYPE

  INTEGER, PARAMETER :: E=3 ! 3=EXPONENT(4.2)

  TYPE(DT0), PARAMETER :: P(4)=DT0()
  TYPE(DT)             :: T1=DT(E1=EXPONENT(P%XR4(1)), E2=EXPONENT(P(:)%XR8(2)), E3=EXPONENT(P(1:)%XR6(4)))

  TYPE(DT0), PARAMETER :: P1(4)=DT0(XR4=0.0, XR6=0.0, XR8=0.0)
  TYPE(DT)             :: T2=DT(EXPONENT(P1%XR4(1)), E2=EXPONENT(P1(:)%XR8(1)), E3=EXPONENT(P1(1:)%XR6(4)))

  REAL, PARAMETER :: NaN=REAL(Z"FFFFFFFF", 4)
  REAL            :: T3=EXPONENT(NaN)


  REAL(4), PARAMETER :: r4_Quiet_NaN      = IEEE_VALUE(0._4, IEEE_QUIET_NAN)
  REAL(4), PARAMETER :: r4_Signaling_NaN  = IEEE_VALUE(0._4, IEEE_SIGNALING_NAN)
  REAL(4), PARAMETER :: r4_Negative_Inf   = IEEE_VALUE(0._4, IEEE_NEGATIVE_INF)
  REAL(4), PARAMETER :: r4_Positive_Inf   = IEEE_VALUE(0._4, IEEE_POSITIVE_INF)

  REAL(8), PARAMETER :: r8_Quiet_NaN      = IEEE_VALUE(0._8, IEEE_QUIET_NAN)
  REAL(8), PARAMETER :: r8_Signaling_NaN  = IEEE_VALUE(0._8, IEEE_SIGNALING_NAN)
  REAL(8), PARAMETER :: r8_Negative_Inf   = IEEE_VALUE(0._8, IEEE_NEGATIVE_INF)
  REAL(8), PARAMETER :: r8_Positive_Inf   = IEEE_VALUE(0._8, IEEE_POSITIVE_INF)

  REAL(4) :: TT1 = EXPONENT(r4_Quiet_NaN)
  REAL(4) :: TT2 = EXPONENT(r4_Signaling_NaN)
  REAL(4) :: TT3 = EXPONENT(r4_Negative_Inf)
  REAL(4) :: TT4 = EXPONENT(r4_Positive_Inf)

  REAL(8) :: TT5 = EXPONENT(r8_Quiet_NaN)
  REAL(8) :: TT6 = EXPONENT(r8_Signaling_NaN)
  REAL(8) :: TT7 = EXPONENT(r8_Negative_Inf)
  REAL(8) :: TT8 = EXPONENT(r8_Positive_Inf)


  IF ( ANY(T1%E1  .NE. E ) )    STOP 11
  IF ( ANY(T1%E2  .NE. E ) )    STOP 12
  IF ( ANY(T1%E3  .NE. E ) )    STOP 13

  IF ( ANY(T2%E1  .NE. 0 ) )    STOP 21
  IF ( ANY(T2%E2  .NE. 0 ) )    STOP 22
  IF ( ANY(T2%E3  .NE. 0 ) )    STOP 23

  IF ( T3  .NE. HUGE(0)  )      STOP 31  ! seems not documented on this

  IF ( TT1  .NE. HUGE(0_4) ) STOP 41
  IF ( TT2  .NE. HUGE(0_4) ) STOP 42
  IF ( TT3  .NE. HUGE(0_4) ) STOP 43
  IF ( TT4  .NE. HUGE(0_4) ) STOP 44
  IF ( TT5  .NE. HUGE(0_4) ) STOP 45
  IF ( TT6  .NE. HUGE(0_4) ) STOP 46
  IF ( TT7  .NE. HUGE(0_4) ) STOP 47
  IF ( TT8  .NE. HUGE(0_4) ) STOP 48


  END


