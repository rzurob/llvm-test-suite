! GB DTP extension using:
! ftcx_dtp -ql -qreuse=self /tstdev/F2003/initExp/Def/InitExpDefElemSIGN.f
! opt variations: -qnol -qreuse=none

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
!*  -  SIGN
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890




  PROGRAM  InitExpDefElemSIGN
  IMPLICIT NONE
  INTEGER :: I, J


  REAL(4), PARAMETER :: r4Min_N           = REAL(z"80800000", KIND=4)
  REAL(4), PARAMETER :: r4N_Zero          = REAL(z"80000000", KIND=4)
  REAL(4), PARAMETER :: r4P_Zero          = REAL(z"00000000", KIND=4)
  REAL(4), PARAMETER :: r4Min_P           = REAL(z"00800000", KIND=4)


  REAL(8), PARAMETER :: r8Min_N           = REAL(z"8080000000000000", KIND=8)
  REAL(8), PARAMETER :: r8N_Zero          = REAL(z"8000000000000000", KIND=8)
  REAL(8), PARAMETER :: r8P_Zero          = REAL(z"0000000000000000", KIND=8)
  REAL(8), PARAMETER :: r8Min_P           = REAL(z"0080000000000000", KIND=8)



  TYPE :: DT4(N1,K1)    ! (20,4)
    INTEGER, KIND :: K1
    INTEGER, LEN  :: N1
    REAL(K1)      :: R41(4,4)=r4Min_N
    REAL(K1)      :: R42(4,4)=r4N_Zero
    REAL(K1)      :: R43(4,4)=r4P_Zero
    REAL(K1)      :: R44(4,4)=r4Min_P
    REAL(K1)      :: R45(4,4)=-4.0
  END TYPE

  TYPE :: DT8(N2,K2)    ! (20,8)
    INTEGER, KIND :: K2
    INTEGER, LEN  :: N2
    REAL(K2)      :: R81(4,4)=r8Min_N
    REAL(K2)      :: R82(4,4)=r8N_Zero
    REAL(K2)      :: R83(4,4)=r8P_Zero
    REAL(K2)      :: R84(4,4)=r8Min_P
    REAL(K2)      :: R85(4,4)=4.0
  END TYPE

  TYPE (DT4(20,4)), PARAMETER :: X4 = DT4(20,4)()
  TYPE (DT4(20,4))            :: T4 = DT4(20,4)(                                    &
                                     R41 = SIGN(A=X4%R41, B=r4Min_P),   &
                                     R42 = SIGN(A=X4%R42, B=r4P_Zero),  &
                                     R43 = SIGN(A=X4%R43, B=r4N_Zero),  &
                                     R44 = SIGN(A=X4%R44, B=r4Min_N),   &
                                     R45 = SIGN(A=X4%R45, B=+0.0  )     &
                                   )


  TYPE (DT8(20,8)), PARAMETER :: X8 = DT8(20,8)()
  TYPE (DT8(20,8))            :: T8 = DT8(20,8)(                                    &
                                     R81 = SIGN(A=X8%R81, B=r8Min_P),   &
                                     R82 = SIGN(A=X8%R82, B=r8P_Zero),  &
                                     R83 = SIGN(A=X8%R83, B=r8N_Zero),  &
                                     R84 = SIGN(A=X8%R84, B=r8Min_N),   &
                                     R85 = SIGN(A=X8%R85, B=-0.0_8)     &
                                   )


  !print*, r4Min_P
  !print*, T4%R41

  IF (ANY( T4%R41  .NE. r4Min_P  ))  ERROR STOP 11
  IF (ANY( T4%R42  .NE. r4P_Zero  )) ERROR STOP 12
  IF (ANY( T4%R43  .NE. r4N_Zero  )) ERROR STOP 13
  IF (ANY( T4%R44  .NE. r4Min_N  ))  ERROR STOP 14
  IF (ANY( T4%R45  .NE. 4.0  ))      ERROR STOP 15

  IF (ANY( T8%R81  .NE. r8Min_P  ))  ERROR STOP 21
  IF (ANY( T8%R82  .NE. r8P_Zero  )) ERROR STOP 22
  IF (ANY( T8%R83  .NE. r8N_Zero  )) ERROR STOP 23
  IF (ANY( T8%R84  .NE. r8Min_N  ))  ERROR STOP 24
  IF (ANY( T8%R85  .NE. -4.0 ))      ERROR STOP 25





  END

