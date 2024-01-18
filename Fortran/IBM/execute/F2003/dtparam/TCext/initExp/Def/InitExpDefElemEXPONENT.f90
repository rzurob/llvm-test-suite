! GB DTP extension using:
! ftcx_dtp -ql -qreuse=self /tstdev/F2003/initExp/Def/InitExpDefElemEXPONENT.f
! opt variations: -qnol -qreuse=none

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
!*  -  EXPONENT
!*  (318985/319550)
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM  InitExpDefElemEXPONENT
  IMPLICIT NONE
  INTEGER :: I, J, K


  TYPE :: DT0(N1,K1,K2,K3)    ! (20,4,8,16)
    INTEGER, KIND :: K1,K2,K3
    INTEGER, LEN  :: N1
    REAL(K1)      :: XR4(4) =  4.2
    REAL(K2)      :: XR8(4) =  4.2
    REAL(K3)      :: XR6(4) =  4.2
  END TYPE

  TYPE :: DT(N2,K4)    ! (20,4)
    INTEGER, KIND :: K4
    INTEGER, LEN  :: N2
    INTEGER(K4)   :: E1(4)
    INTEGER(K4)   :: E2(4)
    INTEGER(K4)   :: E3(4)
  END TYPE

  INTEGER, PARAMETER :: E=3 ! 3=EXPONENT(4.2)

  TYPE(DT0(20,4,8,16)), PARAMETER :: P(4)=DT0(20,4,8,16)()
  TYPE(DT(20,4))             :: T1=DT(20,4)(E1=EXPONENT(P%XR4(1)), E2=EXPONENT(P(:)%XR8(2)), E3=EXPONENT(P(1:)%XR6(4)))

  TYPE(DT0(20,4,8,16)), PARAMETER :: P1(4)=DT0(20,4,8,16)(XR4=0.0, XR6=0.0, XR8=0.0)
  TYPE(DT(20,4))             :: T2=DT(20,4)(EXPONENT(P1%XR4(1)), E2=EXPONENT(P1(:)%XR8(1)), E3=EXPONENT(P1(1:)%XR6(4)))

  REAL, PARAMETER :: NaN=REAL(Z"FFFFFFFF", 4)
  REAL            :: T3=EXPONENT(NaN)


  IF ( ANY(T1%E1  .NE. E ) )    STOP 11
  IF ( ANY(T1%E2  .NE. E ) )    STOP 12
  IF ( ANY(T1%E3  .NE. E ) )    STOP 13

  IF ( ANY(T2%E1  .NE. 0 ) )    STOP 21
  IF ( ANY(T2%E2  .NE. 0 ) )    STOP 22
  IF ( ANY(T2%E3  .NE. 0 ) )    STOP 23

  IF ( T3  .NE. HUGE(0)  )      STOP 31  ! seems not documented on this

  END


